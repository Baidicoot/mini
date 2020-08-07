{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Frontend.Infer where

import Types.Graph
import Types.Type
import Types.Pattern
import Types.Syntax
import Types.Ident

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Constraint = (Type, Type)

data Scheme = Forall [Name] Type deriving(Eq, Show)

newtype Env = TypeEnv (Map.Map Identifier Scheme)

type Subst = Map.Map Name Type

data TypeError
    = UnificationFail Type Type
    | InfiniteType Name Type
    | UnboundIdent Identifier
    | Ambigious [Constraint]
    | UnificationMismatch [Type] [Type]
    | PatternRebind Name
    deriving(Eq, Show)

type Infer = RWST
    Env
    [Constraint]
    InferState
    (Except TypeError)

runInfer :: Infer a -> Env -> Either TypeError (a, InferState, [Constraint])
runInfer i e = runExcept $ runRWST i e startState

type InferState = [Name]

startState :: InferState
startState = [1..] >>= flip replicateM ['a'..'z']

emit :: Type -> Type -> Infer ()
emit a b = tell [(a, b)]

inEnv :: [(Identifier, Scheme)] -> Infer a -> Infer a
inEnv ns m = do
    let scope (TypeEnv e) = TypeEnv $ Map.fromList ns `Map.union` e
    local scope m

lookupEnv :: Identifier -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Just x -> instantiate x
        _ -> throwError $ UnboundIdent x

fresh :: Infer Type
fresh = do
    (n:ns) <- get
    put ns
    pure (Node (TypeVar n))

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Name

applyN :: Subst -> TypeNode -> Type
applyN s t@(TypeVar n) = Map.findWithDefault (Node t) n s
applyN _ t = (Node t)

ftvN :: TypeNode -> Set.Set Name
ftvN (TypeVar n) = Set.singleton n
ftvN _ = Set.empty

instance Substitutable s => Substitutable (AppGraph s) where
    apply s = fmap (apply s)
    ftv = foldr mappend mempty . fmap ftv

instance {-# OVERLAPPING #-} Substitutable Type where -- pretty sure they aren't overlapping?
    apply s = join . fmap (applyN s)
    ftv = foldr mappend mempty . fmap ftvN

instance Substitutable Scheme where
    apply s (Forall ns t) = Forall ns (apply s t)
    ftv (Forall ns t) = ftv t `Set.difference` Set.fromList ns

instance Substitutable Constraint where
    apply s (t0, t1) = (apply s t0, apply s t1)
    ftv (t0, t1) = ftv t0 `Set.union` ftv t1

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

boundIn :: Pattern -> Infer (Set.Set Name)
boundIn (Node (PatternVar n)) = pure (Set.singleton n)
boundIn (App a b) = do
    an <- boundIn a
    bn <- boundIn b
    pure (an `Set.union` bn)
boundIn _ = pure Set.empty

class Inferable i where
    infer :: i -> Infer Type

func :: [Type] -> Type -> Type
func args out = foldr (\arg t -> App (App (Node FunctionType) arg) t) out args

instance Inferable ExprNode where
    infer (Var x) = lookupEnv x
    infer (Lambda (Lam ns e)) = do
        args <- mapM (\n -> do
            f <- fresh
            pure (LocalIdentifier n, Forall [] f)) ns
        t <- inEnv args (infer e)
        pure $ func (fmap (\(_, Forall _ t) -> t) args) t
    infer (Annot (Expl e t)) = do
        t0 <- infer e
        emit t t0
        pure t
    infer (Switch (Match e cs)) = do
        t <- infer e
        n <- fresh
        mapM_ (\(p, d) -> do
            (t0, env) <- inferPat p
            env' <- mapM (\(n, t) -> do
                env <- ask
                let s = generalize env t
                pure (LocalIdentifier n, s)) (Map.toList env)
            dt <- inEnv env' (infer d)
            emit t t0
            emit n dt) cs
        pure n
    infer (LetIn (Let defns e)) = do
        let ns = foldr (\(Defn _ n _ _) ns -> n:ns) [] defns
        nts <- mapM (\n -> do
            f@(Node (TypeVar fv)) <- fresh
            pure (LocalIdentifier n, Forall [] f)) ns
        inEnv nts $ do
            mapM_ (\(Defn _ _ args expr, (_, Forall _ f)) -> do
                argts <- mapM (\n -> do
                    f <- fresh
                    pure (LocalIdentifier n, Forall [] f)) args
                t <- inEnv argts $ infer expr
                emit f $ func (fmap (\(_, Forall _ t) -> t) argts) t) (defns `zip` nts)
            infer e

inferPatNode :: PatternNode -> Map.Map Name Type -> Infer (Type, Map.Map Name Type)
inferPatNode (PatternVar n) env =
    case Map.lookup n env of
        Nothing -> do
            f <- fresh
            pure (f, Map.insert n f env)
        Just _ -> throwError (PatternRebind n)
inferPatNode PatternWildcard env = do
    f <- fresh
    pure (f, env)
inferPatNode (PatternCons id) env = do
    t <- lookupEnv id
    pure (t, env)

inferPat :: Pattern -> Infer (Type, Map.Map Name Type)
inferPat = internal Map.empty
    where
        internal :: Map.Map Name Type -> Pattern -> Infer (Type, Map.Map Name Type)
        internal env (App a b) = do
            (t0, env') <- internal env a
            (t1, env'') <- internal env' b
            t <- fresh
            emit t0 (App (App (Node FunctionType) t1) t)
            pure (t, env'')
        internal env (Node p) = inferPatNode p env

instance Inferable Pattern where
    infer p = do
        (t, _) <- inferPat p
        pure t

instance Inferable i => Inferable (AppGraph i) where
    infer (App a b) = do
        t0 <- infer a
        t1 <- infer b
        t <- fresh
        emit t0 (App (App (Node FunctionType) t1) t)
        pure t
    infer (Node i) = infer i