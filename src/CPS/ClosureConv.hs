{-# LANGUAGE LambdaCase #-}

module CPS.ClosureConv (closureConvert) where

import Types.CPS
import Types.Ident
import Types.Prim

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Maybe
import Data.Foldable

import CPS.Meta

import qualified Data.Set as Set
import qualified Data.Map as Map

type ClosureEnv =
--  current function
    ( Maybe Identifier
--  function pointers
    , Map.Map Name Name
--  free variable renames
    , Map.Map Name Name
--  current closures
    , Map.Map Name Name
--      known functions
    ,   ( Set.Set Identifier
--      escaping functions
        , Set.Set Identifier
--      per-function data
        , Map.Map Identifier FunctionMeta
--      closure data
        , Map.Map Identifier FunctionClosure
        )
    )

type ClosureState =
--  fresh variable name
    ( Int
--  split function names
    , Map.Map Name Name
    )

type ClosureConv = ReaderT ClosureEnv (State ClosureState)

closureConvert :: CExp -> Int -> CExp
closureConvert exp i
    = uncurry Fix
    . smash . fst
    . flip runState (i, mempty)
    . runReaderT (convertExp exp)
    $ let fnMeta = collect exp in
        (Nothing, mempty, mempty, mempty, (allKnown fnMeta, allEscaping fnMeta, fnMeta, reduce fnMeta))

fresh :: ClosureConv Name
fresh = do
    (n, s) <- get
    put (n+1, s)
    pure ('c':show n)

split :: Name -> ClosureConv Name
split id = do
    (_, splits) <- get
    case Map.lookup id splits of
        Just n  -> pure n
        Nothing -> do
            f <- fresh
            modify (\(a,b) -> (a,Map.insert id f b))
            pure f

currentData :: ClosureConv (Maybe FunctionMeta)
currentData = do
    (n, _, _, _, (_, _, d, _)) <- ask
    pure (n >>= flip Map.lookup d)

extraVars :: Name -> ClosureConv [Name]
extraVars id = do
    (_, _, _, _, (_, _, _, c)) <- ask
    pure . (\(FunctionClosure s) -> Set.toList s) $ Map.findWithDefault (FunctionClosure mempty) (LocalIdentifier id) c

known :: Name -> ClosureConv Bool
known n = do
    (_, _, _, _, (k, _, _, _)) <- ask
    pure $ (LocalIdentifier n) `Set.member` k

escapes :: Name -> ClosureConv Bool
escapes f = do
    (n, _, _, _, (_, _, d, _)) <- ask
    case n of
        Just n  -> case Map.lookup n d of
            Just d  -> pure ((LocalIdentifier f) `Set.member` escaping d)
            Nothing -> pure False
        Nothing -> pure False

isEscaping :: Identifier -> ClosureConv Bool
isEscaping i = do
    (_, _, _, _, (_, e, _, _)) <- ask
    pure (i `Set.member` e)

called :: Name -> ClosureConv Bool
called f = do
    (n, _, _, _, (_, _, d, _)) <- ask
    case n of
        Just n  -> case Map.lookup n d of
            Just d  -> pure (f `Set.member` unknownCalls d)
            Nothing -> pure False
        Nothing -> pure False

name :: Name -> ClosureConv Name
name n = do
    (_, _, r, _, _) <- ask
    case Map.lookup n r of
        Just x  -> pure x
        Nothing -> pure n

value :: Value -> ClosureConv Value
value v@(Var (LocalIdentifier n)) = do
    (_, _, r, _, _) <- ask
    case Map.lookup n r of
        Just x  -> pure . Var $ LocalIdentifier x
        Nothing -> pure v
value v = pure v

fnPtr :: Value -> ClosureConv Value
fnPtr v@(Var (LocalIdentifier n)) = do
    (_, f, _, _, _) <- ask
    case Map.lookup n f of
        Just x  -> pure . Var $ LocalIdentifier x
        Nothing -> pure v
fnPtr v = pure v

hasCls :: Name -> ClosureConv Bool
hasCls n = do
    (_, _, _, c, _) <- ask
    pure (n `Map.member` c)

closure :: Value -> ClosureConv Value
closure v@(Var (LocalIdentifier n)) = do
    (_, _, _, c, _) <- ask
    case Map.lookup n c of
        Just x  -> pure . Var $ LocalIdentifier x
        Nothing -> pure v
closure v = pure v

arg :: Value -> ClosureConv Value
arg = closure >=> value

withPtr :: Name -> ClosureConv CExp -> ClosureConv CExp
withPtr f c = do
    fp <- fresh
    f' <- name f
    exp <- local (\(a,b,c,d,e) -> (a,Map.insert f fp b,c,d,e)) c
    pure $ Select 0 (Var $ LocalIdentifier f') fp exp

withCls :: Name -> ClosureConv CExp -> ClosureConv CExp
withCls f c = do
    fknown <- known f
    fcls <- hasCls f
    if fknown && not fcls then do
        fs <- split f
        fc <- fresh
        ffv <- mapM name =<< extraVars f
        exp <- local (\(a,b,c,d,e) -> (a,b,c,Map.insert f fc d,e)) c
        pure $ Record (fmap (\f -> (Var $ LocalIdentifier f, OffPath 0)) (fs:ffv)) fc exp
    else
        c

argCls :: [Value] -> ClosureConv CExp -> ClosureConv CExp
argCls vs c = do
    ns <- filterM escapes (extractNames vs)
    foldr (\a -> (. withCls a)) id ns c

rename :: Name -> Name -> ClosureConv CExp -> ClosureConv CExp
rename x y = local (\(a,b,c,d,e) -> (a,b,Map.insert x y c,d,e))

freshen :: Name -> ClosureConv CExp -> ClosureConv CExp
freshen n c = do
    n' <- fresh
    rename n n' c

extractPtrs :: [Name] -> ClosureConv CExp -> ClosureConv CExp
extractPtrs ns c = do
    ns <- filterM called ns
    foldr (\a -> (. withPtr a)) id ns c

enterFunction :: Identifier -> ClosureConv a -> ClosureConv a
enterFunction id = local (\(_,_,_,_,meta) -> (Just id,mempty,mempty,mempty,meta))

-- split an escaping (known) function f with directly passed arguments args and closure-passed arguments extra
splitFn :: Name -> [Name] -> [Name] -> ClosureConv CFun
splitFn f args extra = do
    fs <- split f
    c <- fresh
    args' <- mapM (const fresh) args
    extra' <- mapM (const fresh) extra
    let call = App (Var $ LocalIdentifier f) (fmap (Var . LocalIdentifier) $ args' ++ extra')
    let body = foldr (\(i,n) -> Select i (Var $ LocalIdentifier c) n) call (zip [1..] extra')
    pure (Fun (LocalIdentifier fs) (args' ++ [c]) body)

convertExp :: CExp -> ClosureConv CExp
-- local call
convertExp (App v@(Var (LocalIdentifier f)) args) = do
    fknown <- known f
    if fknown then do
        extra <- extraVars f
        argCls (args ++ fmap (Var . LocalIdentifier) extra) $ do
            extra'  <- mapM (arg . Var . LocalIdentifier) extra
            args'   <- mapM arg args
            pure (App v (args' ++ extra'))
    else argCls args $ do
        vp <- fnPtr v
        vc <- value v
        args' <- mapM arg args
        pure (App vp (args' ++ [vc]))
-- top-level call
convertExp (App v@(Var id) args) = argCls args $ do
    args' <- mapM arg args
    pure (App v args')
convertExp (Fix fns exp) = do
    fns' <- mapM (\case
        Fun i@(LocalIdentifier f) args exp -> enterFunction i $ do
            extra <- mapM (\n -> do
                n' <- fresh
                pure (n, n')) =<< extraVars f
            let renamings = foldr (\(n,n') -> (.rename n n')) id extra
            exp' <- renamings . extractPtrs (args ++ fmap fst extra) $ convertExp exp
            pure (Fun (LocalIdentifier f) (args ++ fmap snd extra) exp')
        Fun i args exp -> enterFunction i $ do
            exp' <- extractPtrs args $ convertExp exp
            pure (Fun i args exp')) fns
    splitfns <-
        mapM (\(Fun (LocalIdentifier f) args _) -> splitFn f args =<< extraVars f)
        =<< filterM (\case
            Fun (LocalIdentifier f) _ _ -> isEscaping (LocalIdentifier f)
            _ -> pure False) fns
    exp' <- convertExp exp
    pure (Fix (fns' ++ splitfns) exp')
convertExp (Record vs n exp) = argCls (fmap fst vs) $ do
    vs' <- mapM (\(v,p) -> do
        v' <- arg v
        pure (v',p)) vs
    exp' <- convertExp exp
    pure (Record vs' n exp')
convertExp (Select i v n exp) = do
    v' <- value v
    exp' <- convertExp exp
    pure (Select i v' n exp')
convertExp (Switch v exps) = do
    v' <- value v
    exps' <- mapM convertExp exps
    pure (Switch v' exps')
convertExp (Primop op vs n exps) = argCls vs $ do
    vs' <- mapM arg vs
    exps' <- mapM convertExp exps
    pure (Primop op vs' n exps')
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