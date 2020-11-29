module CPS.ClosureConv (closureConv) where

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
--  function pointers
    ( Map.Map Identifier Value
--  free variable renames
    , Map.Map Identifier Identifier
--  current closures
    , Map.Map Identifier Value
--      escaping functions
    ,   ( Set.Set Identifier
--      captured variables
        , Map.Map Identifier FV
        )
    )

type ClosureState =
--  fresh variable names
    ( Int
--  split function names
    , Map.Map Identifier Identifier
    )

type ClosureConv = ReaderT ClosureEnv (State ClosureState)

closureConv :: CExp -> Int -> (CExp, Int)
closureConv e s =
    let esc = getEscaping e
        cap = getCaptured e
        (e', (s',_)) = runClosureConv (convertExp e) (mempty,mempty,mempty,(esc,cap)) (s,mempty)
        (fns, tail) = smash e'
    in (Fix fns tail, s')

runClosureConv :: ClosureConv a -> ClosureEnv -> ClosureState -> (a, ClosureState)
runClosureConv c e = runState (runReaderT c e)

enterfn :: ClosureConv a -> ClosureConv a
enterfn = local (\(a,b,c,d)->(mempty,mempty,mempty,d))

fresh :: ClosureConv Name
fresh = do
    (n, s) <- get
    put (n+1, s)
    pure ('c':show n)

split :: Identifier -> ClosureConv Identifier
split i = do
    (_, splits) <- get
    case Map.lookup i splits of
        Just n  -> pure n
        Nothing -> do
            f <- case i of
                LocalIdentifier n -> pure ("split_" ++ n)
                ExternalIdentifier _ n -> pure ("split_" ++ n)
            modify (second (Map.insert i (LocalIdentifier f)))
            pure (LocalIdentifier f)

freeVars :: Identifier -> ClosureConv [Identifier]
freeVars i = do
    (_, _, _, (_, c)) <- ask
    pure . Set.toList $ Map.findWithDefault mempty i c

escapingFn :: Identifier -> ClosureConv Bool
escapingFn i = do
    (_, _, _, (e, _)) <- ask
    pure (i `Set.member` e)

renaming :: Identifier -> ClosureConv Identifier
renaming i = do
    (_, r, _, _) <- ask
    case Map.lookup i r of
        Just x  -> pure x
        Nothing -> pure i

value :: Value -> ClosureConv Value
value (Var i) = do
    i' <- renaming i
    pure (Var i')
value v = pure v

fnPtr :: Identifier -> ClosureConv (Maybe Value)
fnPtr i = do
    (f, _, _, _) <- ask
    pure (Map.lookup i f)

fnCls :: Value -> ClosureConv Value
fnCls v@(Label i) = do
    (_, _, c, _) <- ask
    case Map.lookup i c of
        Just v' -> pure v'
        Nothing -> pure v
fnCls v = pure v

arg :: Value -> ClosureConv Value
arg = fnCls >=> value

extractPtr :: Identifier -> ClosureConv CExp -> ClosureConv CExp
extractPtr i c = do
    fp <- fresh
    i' <- renaming i
    Select 0 (Var i') (LocalIdentifier fp) <$> local (\(a,b,c,d) -> (Map.insert i (Var $ LocalIdentifier fp) a,b,c,d)) c

makeCls :: Value -> ClosureConv CExp -> ClosureConv CExp
makeCls (Label i) c = do
    is <- split i
    icls <- fresh
    ffv <- mapM renaming =<< freeVars i
    Record ((Label is, NoPath):fmap (\f -> (Var f, NoPath)) ffv) (LocalIdentifier icls) <$> local (\(a,b,c,d)->(a,b,Map.insert i (Var $ LocalIdentifier icls) c,d)) c
makeCls _ c = c

rename :: Identifier -> Identifier -> ClosureConv CExp -> ClosureConv CExp
rename x y = local (\(a,b,c,d) -> (a,Map.insert x y b,c,d))

-- split an escaping (known) function f with directly passed arguments args and closure-passed arguments extra
splitFn :: Identifier -> Int -> Int -> ClosureConv CFun
splitFn f nargs nextra = do
    fs <- split f
    c <- fresh
    args <- replicateM nargs fresh
    extra <- replicateM nextra fresh
    let call = App (Label f) (fmap (Var . LocalIdentifier) $ args ++ extra)
    let body = foldr (\(i,n) -> Select i (Var $ LocalIdentifier c) (LocalIdentifier n)) call (zip [1..] extra)
    pure (Fun fs (fmap LocalIdentifier args ++ [LocalIdentifier c]) body)

convertExp :: CExp -> ClosureConv CExp
convertExp (App (Label k) args) = flip (foldr makeCls) args $ do
        extra <- fmap (fmap Var) . mapM renaming =<< freeVars k
        args' <- mapM arg args
        pure (App (Label k) (args' ++ extra))
convertExp (App (Var u) args) = flip (foldr makeCls) args $ do
    args' <- mapM arg args
    vp <- fnPtr u
    case vp of
        Just vp -> do
            vc <- value (Var u)
            pure (App vp (args' ++ [vc]))
        Nothing -> do
            u' <- arg (Var u)
            pure (App u' args')
convertExp (Fix fns e) = do
    fns' <- mapM (\(Fun i args e) -> enterfn $ do
        extra <- mapM (\n -> do
            n' <- fresh
            pure (n, n')) =<< freeVars i
        let renamings = foldr (\(n,n') -> (. rename n (LocalIdentifier n'))) id extra
        let extracts = foldr ((.) . extractPtr) id $ filter (calledIn e) (fmap fst extra ++ args)
        e' <- renamings . extracts $ convertExp e
        pure (Fun i (args ++ fmap (LocalIdentifier . snd) extra) e')) fns
    splits <- mapM (\(Fun i args _) -> splitFn i (length args) . length =<< freeVars i)
        =<< filterM (\(Fun i _ _) -> escapingFn i) fns
    e' <- convertExp e
    pure (Fix (fns' ++ splits) e')
convertExp (Record vs n e) = flip (foldr makeCls) (fmap fst vs) $ do
    vs' <- mapM (\(v,p) -> do
        v' <- arg v
        pure (v',p)) vs
    e' <- convertExp e
    pure (Record vs' n e')
convertExp (Select i v n e) = do
    v' <- value v
    e' <- convertExp e
    pure (Select i v' n e')
convertExp (Switch v es) = do
    v' <- value v
    es' <- mapM convertExp es
    pure (Switch v' es')
convertExp (Primop op vs n es) = flip (foldr makeCls) vs $ do
    vs' <- mapM arg vs
    es' <- mapM convertExp es
    pure (Primop op vs' n es')
convertExp x = pure x
{-
convertExp :: CExp -> ClosureConv CExp
-- local call
convertExp (App (Label (LocalIdentifier i)) args) = do
    extra <- extraVars i
    argCls (args ++ fmap (Var . LocalIdentifier) extra) $ do
        extra'  <- mapM (arg . Var . LocalIdentifier) extra
        args'   <- mapM arg args
        pure (App (Label $ LocalIdentifier i) (args' ++ extra'))
convertExp (App v args) = argCls args $ do
        vp <- fnPtr v
        vc <- value v
        args' <- mapM arg args
        pure (App vp (args' ++ [vc]))
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
-}
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