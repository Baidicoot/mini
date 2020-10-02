{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.ClosureConv (closureConvert, mkEnv) where

import System.IO.Unsafe

import Types.CPS
import Types.Ident

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Maybe
import Data.Foldable

import qualified Data.Set as Set
import qualified Data.Map as Map

-- initial pass to gather data about the functions

{-
'Types' of variable:
- free variables - variables that are not bound in the function
- (all) escaping variables - variables that are arguments to operators
- known calls
- unknown calls
- calls that might capture
-}

type Metadata = Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)
-- fv, escaping, known calls, unknown calls, calls that might capture

type CollectEnv =
    (Maybe Identifier, Set.Set Name)
-- current function name, known functions

type Collector = ReaderT CollectEnv (Writer (Set.Set Name))

knownF :: Name -> Collector Bool
knownF n = do
    (_, env) <- ask
    pure $ n `Set.member` env

update :: ((Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name) -> (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)) -> Metadata -> Collector Metadata
update f m = do
    env <- ask
    case env of
        (Nothing, _) -> pure m
        (Just n, _) -> pure $ Map.update (\(a, b, c, d, e) -> Just $ f (a, b, c, d, e)) n m

includes :: Set.Set Name -> Metadata -> Collector Metadata
includes ns m = do
    ns' <- filterM (fmap not . knownF) (Set.toList ns)
    ns'' <- filterM knownF (Set.toList ns)
    update (\(a, b, c, d, e) -> (a `Set.union` (Set.fromList ns'), b, c, d, e `Set.union` (Set.fromList ns''))) m

binds :: Name -> Metadata -> Collector Metadata
binds n = update (\(a, b, c, d, e) -> (n `Set.delete` a, b, c, d, e))

fnArgs :: [Name] -> Metadata -> Collector Metadata
fnArgs ns = update (\(a, b, c, d, e) -> (a `Set.difference` (Set.fromList ns), b, c, d, e))

root :: Collector Metadata
root = do
    env <- ask
    case env of
        (Nothing, _) -> pure mempty
        (Just n, _) -> pure (Map.singleton n mempty)

calls :: Set.Set Name -> Metadata -> Collector Metadata
calls n m = do
    fs <- foldM (\b n -> fmap (. b) $ do
        isKnown <- knownF n
        pure $ if isKnown then
                (\(a, b, c, d, e) -> (a, b, Set.insert n c, d, e))
            else
                (\(a, b, c, d, e) -> (a, b, c, Set.insert n d, e))) id n
    includes n =<< update fs m

hasArgs :: Set.Set Name -> Metadata -> Collector Metadata
hasArgs ns = update (\(a, b, c, d, e) -> (a, b `Set.union` ns, c, d, e))

inFn :: Identifier -> Collector a -> Collector a
inFn a = local (\(_, b) -> (Just a, b))

withKnown :: Set.Set Name -> Collector a -> Collector a
withKnown ns = local (\(a, b) -> (a, b `Set.union` ns))

collectM :: CExp -> Collector Metadata
collectM (Fix fns exp) = do
    let names = Set.fromList . catMaybes $ fmap (\(Fun id _ _) -> identToName id) fns
    tell names
    m <- collectM exp
    withKnown names $ foldM (\m (Fun id args exp) -> fmap (mappend m) . inFn id $ fnArgs args =<< collectM exp) m fns
collectM (App v vs) = do
    let vs' = valuesToSet vs
    let v' = valueToSet v
    calls v' =<< hasArgs vs' =<< root
collectM (Record paths n exp) = do
    let vs = valuesToSet $ fmap fst paths
    hasArgs vs =<< binds n =<< collectM exp
collectM (Select _ v n exp) = do
    let v' = valueToSet v
    hasArgs v' =<< binds n =<< collectM exp
collectM (Switch v exps) = do
    let v' = valueToSet v
    hasArgs v' =<< foldM (\b -> fmap (mappend b) . collectM) mempty exps
collectM (Primop _ vs n exps) = do
    let vs' = valuesToSet vs
    hasArgs vs' =<< binds n =<< foldM (\b -> fmap (mappend b) . collectM) mempty exps
collectM _ = root

collect :: CExp -> (Metadata, Set.Set Name)
collect e = runWriter $ runReaderT (collectM e) (Nothing, mempty)

type ClosureData = Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)
-- fv, escaping, known calls, unknown calls

reduce :: Metadata -> ClosureData
reduce = fmap (\(a, b, c, d, _) -> (a, b, c, d)) . reduceFully
    where
        reduceFully m = let m' = reduceOnce m in if m == m' then m else reduceFully m'
        reduceOnce m = fmap (\(fv, b, c, d, needs) ->
            let (fv', needs') = foldr mappend (fv, needs) . fmap (\need -> case Map.lookup (LocalIdentifier need) m of
                    Just (fv, _, _, _, needs) -> (fv, needs)
                    Nothing -> mempty) $ Set.toList needs
            in (fv', b, c, d, needs')) m

type ClosureEnv = (Maybe Identifier, Map.Map Name Name, Map.Map Name Name, Set.Set Name, ClosureData)
-- current function, current renamings, current function pointers, known functions, freevars of functions that need them

collate :: (ClosureData, Set.Set Name) -> ClosureEnv
collate (cd, kf) = (Nothing, mempty, mempty, kf, cd)

mkEnv = collate . first reduce . collect

type ClosureState = [Name]

type ClosureConv = ReaderT ClosureEnv (State ClosureState)

closureConvert :: CExp -> CExp
closureConvert exp = uncurry Fix . smash . fst . flip runState cconvNames $ runReaderT (convertExp exp) (collate . first reduce $ collect exp)

cconvNames :: [Name]
cconvNames = [1..] >>= flip replicateM ['a'..'z']

fresh :: ClosureConv Name
fresh = do
    names <- get
    let (n:ns) = names
    put ns
    pure n

mtf :: Monad m => (m a, b) -> m (a, b)
mtf (a, b) = do
    a' <- a
    pure (a', b)

split :: Name -> Name
split = (++"_")

getCurrentMeta :: ClosureConv (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)
getCurrentMeta = do
    (n, _, _, _, env) <- ask
    case n of
        Nothing -> pure mempty
        Just n -> pure $ Map.findWithDefault mempty n env

escapes :: Name -> ClosureConv Bool
escapes n = do
    (_, esc, _, _) <- getCurrentMeta
    pure $ n `Set.member` esc

known :: Name -> ClosureConv Bool
known n = do
    (_, _, _, k, _) <- ask
    pure $ n `Set.member` k

calledUnknown :: Name -> ClosureConv Bool
calledUnknown n = do
    (_, _, a, b) <- getCurrentMeta
    pure $ n `Set.member` b

calledKnown :: Name -> ClosureConv Bool
calledKnown n = do
    (_, _, a, b) <- getCurrentMeta
    pure $ n `Set.member` a

getFv :: Name -> ClosureConv [Name]
getFv n = do
    (_, _, _, _, env) <- ask
    pure . Set.toList . (\(fv, _, _, _) -> fv) $ Map.findWithDefault mempty (LocalIdentifier n) env

getFV :: Value -> ClosureConv [Name]
getFV (Var (LocalIdentifier n)) = getFv n
getFV _ = pure []

needsClosure :: Name -> ClosureConv Bool
needsClosure n = do
    (_, _, _, _, m) <- ask
    let (f, _, _, _) = Map.findWithDefault mempty (LocalIdentifier n) m
    pure . not $ Set.null f

getName :: Name -> ClosureConv Name
getName n = do
    (_, ns, _, _, _) <- ask
    pure $ Map.findWithDefault n n ns

getVal :: Value -> ClosureConv Value
getVal (Var (LocalIdentifier n)) = fmap (Var . LocalIdentifier) (getName n)
getVal x = pure x

getFnPtr :: Name -> ClosureConv Name
getFnPtr n = do
    (_, _, ns, _, _) <- ask
    pure $ Map.findWithDefault "unknown" n ns

getPtr :: Value -> ClosureConv (Maybe Value)
getPtr (Var (LocalIdentifier n)) = fmap (Just . Var . LocalIdentifier) $ getFnPtr n
getPtr _ = pure Nothing

-- rewrite EVERYTHING - *rename* everything

withFp :: Name -> ClosureConv CExp -> ClosureConv CExp
withFp f c = do
    f' <- getName f
    fp <- fresh
    exp <- local (\(a, b, c, d, e) -> (a, b, Map.insert f fp c, d, e)) c
    pure $ Select 0 (Var $ LocalIdentifier f') fp exp

withCls :: Name -> ClosureConv CExp -> ClosureConv CExp
withCls f c = do
    f' <- getName f
    fr <- fresh
    ffv <- mapM getName =<< getFv f
    exp <- local (\(a, b, c, d, e) -> (a, Map.insert f fr b, c, d, e)) c
    pure $ Record (fmap (\f -> (Var $ LocalIdentifier f, OffPath 0)) (split f':ffv)) fr exp

withArgs :: [Value] -> ClosureConv CExp -> ClosureConv CExp
withArgs vs c = do
    fns <- mapM (\case
        Var (LocalIdentifier n) -> do
            esc <- escapes n
            needs <- needsClosure n
            pure $ if esc && needs then
                    withCls n
                else
                    id
        _ -> pure id) vs
    foldr ($) c fns

bind :: Name -> Name -> ClosureConv a -> ClosureConv a
bind f f' = local (\(a, b, c, d, e) -> (a, Map.insert f f' b, c, d, e))

getPtrs :: [Name] -> CExp -> ClosureConv CExp
getPtrs aargs c = do
    cls <- filterM calledUnknown aargs
    let cfns = fmap withFp cls
    foldr (.) id cfns (convertExp c)

localFn :: Name -> [Name] -> CExp -> ClosureConv CFun
localFn n args c = local (\(_, b, c, d, e) -> (Just (LocalIdentifier n), b, c, d, e)) $ do
    n' <- getName n
    ffv <- getFv n
    let aargs = args ++ ffv
    argM <- mapM (\a -> fmap ((,) a) fresh) aargs
    exp' <- local (\(a, b, c, d, e) -> (a, Map.union (Map.fromList argM) b, c, d, e)) $ getPtrs aargs c
    pure $ Fun (LocalIdentifier n') (fmap snd argM) exp'

globalFn :: Identifier -> [Name] -> CExp -> ClosureConv CFun
globalFn id args c = local (\(_, b, c, d, e) -> (Just id, b, c, d, e)) $ do
    argM <- mapM (\a -> fmap ((,) a) fresh) args
    exp' <- local (\(a, b, c, d, e) -> (a, Map.union (Map.fromList argM) b, c, d, e)) $ getPtrs args c
    pure $ Fun id (fmap snd argM) exp'

convertFun :: CFun -> ClosureConv CFun
convertFun (Fun (LocalIdentifier n) args exp) = localFn n args exp
convertFun (Fun id args exp) = globalFn id args exp

splitFn :: Name -> [Name] -> ClosureConv CFun
splitFn f args = do
    ffv <- getFv f
    f' <- getName f
    c <- fresh
    fv' <- mapM (\a -> fresh) ffv
    arg' <- mapM (\a -> fresh) args
    let binding = foldr (.) id (fmap (\(n,i) -> Select i (Var $ LocalIdentifier c) n) (zip fv' [1..]))
    pure . Fun (LocalIdentifier (split f')) (arg' ++ [c]) . binding $ App (Var $ LocalIdentifier f') (fmap (Var . LocalIdentifier) $ arg' ++ fv')

convertExp :: CExp -> ClosureConv CExp
convertExp (Fix defs exp) = do
    let fnNamesArgs = concatMap (\(Fun id args _) -> case id of
            LocalIdentifier n -> [(n,args)]
            _ -> []) defs
    fnM <- mapM (\(n,_) -> fmap ((,) n) fresh) fnNamesArgs
    local (\(a, b, c, d, e) -> (a, Map.union (Map.fromList fnM) b, c, d, e)) $ do
        needsSplit <- filterM (needsClosure. fst) fnNamesArgs
        splitted <- mapM (uncurry splitFn) needsSplit
        defs' <- mapM convertFun defs
        exp' <- convertExp exp
        pure $ Fix (defs'++splitted) exp'
convertExp (App v vs) = withArgs vs $ do
    mp <- getPtr v
    v' <- getVal v
    vs' <- mapM getVal vs
    case mp of
        Just ptr -> pure $ App ptr (vs'++[v'])
        Nothing -> do
            ffv <- getFV v
            vfv <- mapM (fmap (Var . LocalIdentifier) . getName) ffv
            pure $ App v' (vs'++vfv)
convertExp (Record paths n exp) = withArgs (fmap fst paths) $ do
    n' <- fresh
    paths' <- mapM (mtf . first getVal) paths
    exp' <- bind n n' (convertExp exp)
    pure $ Record paths' n' exp'
convertExp (Select i v n exp) = withArgs [v] $ do
    v' <- getVal v
    n' <- fresh
    exp' <- bind n n' (convertExp exp)
    pure $ Select i v' n' exp'
convertExp (Switch v exps) = do -- can only 'Switch' on an int
    v' <- getVal v
    exps' <- mapM convertExp exps
    pure $ Switch v' exps'
convertExp (Primop p vs n exps) = withArgs vs $ do
    vs' <- mapM getVal vs
    n' <- fresh
    exps' <- bind n n' $ mapM convertExp exps
    pure $ Primop p vs' n' exps'
convertExp x = pure x

smash :: CExp -> ([CFun], CExp)
smash (Fix defs exp) =
    let defs' = concatMap (\(Fun id args exp) ->
            let (defs, exp') = smash exp in (Fun id args exp'):defs) defs
    in first (++defs') (smash exp)
smash (Record a b exp) = second (Record a b) (smash exp)
smash (Select a b c exp) = second (Select a b c) (smash exp)
smash (Switch a exps) = second (Switch a) . foldr (mappend . (\(a, b) -> (a, [b]))) mempty $ fmap smash exps
smash (Primop a b c exps) = second (Primop a b c) . foldr (mappend . (\(a, b) -> (a, [b]))) mempty $ fmap smash exps
smash x = ([], x)

{-
encloseArg :: Value -> ClosureConv CExp -> ClosureConv CExp
encloseArg (Var (LocalIdentifier n)) c = do
    enclose <- needsClosure n
    if enclose then do
        v <- freshCls n
        fvs <- mapM getName =<< getFv n
        let binding = Record (fmap (flip (,) (OffPath 0) . Var . LocalIdentifier) (split v:fvs)) v
        fmap binding $ local (\(a, b, c, d, e) -> (a, Map.insert n v b, c, d, e)) c
    else
        c
encloseArg _ c = c

-- need to sort out old/new args
convertFun :: CFun -> ClosureConv CFun
convertFun (Fun (LocalIdentifier n) args exp) = do
    enclose <- needsClosure n
    (newargs, oldargs, localfn) <- if enclose then do
            fv <- mapM (\n -> fmap ((,) n) (freshFv n)) =<< getFv n
            pure (args ++ fmap snd fv, args ++ fmap fst fv, (\(_, b, c, d, e) -> (Just n, (Map.fromList fv) `Map.union` b, c, d, e)))
        else
            pure (args, args, (\(_, b, c, d, e) -> (Just n, b, c, d, e)))
    local localfn $ do
        (binding, fps) <- fmap (foldr (\(a, b) (a', b') -> (a . a', b ++ b')) (id, [])) $ mapM (\f -> do
            called <- calledUnknown f
            if called then do
                f' <- getName f
                fp <- freshFnPtr f
                let binding = Select 0 (Var $ LocalIdentifier f') fp
                pure (binding, [(f, fp)])
            else
                pure (id, [])) oldargs
        exp' <- local (\(a, b, c, d, e) -> (a, b, (Map.fromList fps) `Map.union` c, d, e)) (convertExp exp)
        pure . Fun (LocalIdentifier n) newargs $ binding exp'
convertFun (Fun id args exp) = do
    exp' <- local (\(_, b, c, d, e) -> (Nothing, b, c, d, e)) (convertExp exp)
    pure $ Fun id args exp'

splitFun :: Name -> [Name] -> ClosureConv CFun
splitFun f args = do
    fv <- getFv f
    cls <- freshCls f
    fargs <- mapM (\n -> freshFv n) fv
    let args' = args++[cls]
    let binding = foldr (.) id (fmap (\(n, i) -> Select i (Var $ LocalIdentifier cls) n) (zip fargs [1..]))
    pure . Fun (LocalIdentifier $ split f) args' . binding . App (Var $ LocalIdentifier f) . fmap (Var . LocalIdentifier) $ args ++ fargs

convertExp :: CExp -> ClosureConv CExp
convertExp (App (Var (LocalIdentifier f)) args) = foldr (.) id (fmap encloseArg args) $ do
    fKnown <- known f
    args' <- mapM getVal args
    if fKnown then
        pure (App (Var $ LocalIdentifier f) args')
    else do
        f' <- getName f
        fp <- getFnPtr f
        pure . App (Var $ LocalIdentifier fp) $ args' ++ [Var $ LocalIdentifier f']
convertExp (Record paths n exp) = foldr (.) id (fmap (encloseArg . fst) paths) $ do
    paths' <- mapM (\(v, p) -> fmap (flip (,) p) (getVal v)) paths
    exp' <- convertExp exp
    pure (Record paths' n exp')
convertExp (Select i v n exp) = encloseArg v $ do
    v' <- getVal v
    exp' <- convertExp exp
    pure (Select i v' n exp')
convertExp (Switch v exps) = -- since you cannot pattern match on a function, v cannot need a closure
    fmap (Switch v) (mapM convertExp exps)
convertExp (Primop p args n exps) = foldr (.) id (fmap encloseArg args) $ do
    args' <- mapM getVal args
    fmap (Primop p args' n) (mapM convertExp exps)
convertExp (Fix defs exp) = do
    defs' <- mapM convertFun defs
    splitted <- fmap (foldr mappend []) $ mapM (\case
        Fun (LocalIdentifier n) args _ -> fmap (\a -> [a]) $ splitFun n args
        _ -> pure []) defs
    exp' <- convertExp exp
    pure (Fix (defs'++splitted) exp)
convertExp x = pure x
-}