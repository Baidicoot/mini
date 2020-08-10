{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Frontend.Constraint (
    Constraint(..),
    Scheme(..),
    Substitutable(..),
    Subst,
    TypeError(..),
    TaggedIR,
    TaggedIRNode,
    runInfer,
    infer,
    generalize
) where

-- derived from: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf

import Types.Graph
import Types.Type
import Types.Pattern
import Types.Ident
import Types.IR

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)

data Constraint
    = Unify Type Type
    | Inst Type Type Monomorphic
    | Rigid Type Scheme
    deriving(Eq)

instance Show Constraint where
    show (Unify t0 t1) = show t0 ++ " ~~ " ++ show t1
    show (Inst t0 t1 m) = show t0 ++ " <~{" ++ intercalate "," (Set.toList m) ++ "} " ++ show t1
    show (Rigid t0 s) = show t0 ++ " <~ " ++ show s

type AssumptionSet = [(Identifier, Type)]

instance Substitutable Constraint where
    apply s (Unify t0 t1) = Unify (apply s t0) (apply s t1)
    apply s (Inst t0 t1 m) = Inst (apply s t0) (apply s t1) sm
        where
            sm = Set.unions . Set.map (\n -> ftv $ Map.findWithDefault (Node () (TypeVar n)) n s) $ m
    apply s (Rigid tau sigma) = Rigid (apply s tau) (apply s sigma)

    ftv (Unify t0 t1) = ftv t0 `Set.union` ftv t1
    ftv (Inst t0 t1 m) = ftv t0 `Set.union` (ftv t1 `Set.difference` m)
    ftv (Rigid tau sigma) = ftv tau `Set.union` ftv sigma

data TypeError
    = UnificationFail Type Type
    | InfiniteType Name Type
    | Ambigious [Constraint]
    | UnificationMismatch [Type] [Type]
    deriving(Eq, Show)

type InferState = ([Name], AssumptionSet)

type Infer = RWST
    Monomorphic
    [Constraint]
    InferState
    (Except TypeError)

runInfer :: Infer a -> [Name] -> Either TypeError (a, [Name], AssumptionSet, [Constraint])
runInfer a s = (\(a, (s, as), cs) -> (a, s, as, cs)) <$> runExcept (runRWST a Set.empty (s, []))

fresh :: Infer Name
fresh = do
    (n:ns, as) <- get
    put (ns, as)
    pure n

assume :: Identifier -> Type -> Infer ()
assume n t = do
    (ns, as) <- get
    put (ns, (n, t):as)

abstract :: Name -> Type -> Infer ()
abstract n t = do
    (_, as) <- get
    mapM_ (constrain . Unify t . snd) $ filter ((==(LocalIdentifier n)) . fst) as

letabst :: Name -> Type -> Infer ()
letabst n t = do
    mono <- ask
    (_, as) <- get
    mapM_ (constrain . (\t0 -> Inst t0 t mono) . snd) $ filter ((==(LocalIdentifier n)) . fst) as

constrain :: Constraint -> Infer ()
constrain x = tell [x]

inEnv :: (Set.Set Name) -> Infer a -> Infer a
inEnv n p = local (Set.union n) p

infixr 9 -->

(-->) :: Type -> Type -> Type
a --> b = App () (App () (Node () FunctionType) a) b

functionkind :: Kind
functionkind = (Node () KindStar) --> (Node () KindStar) --> (Node () KindStar)

class Inferable i o where
    infer :: i -> Infer (Type, o)

type TaggedIRNode = PolyIRNode Scheme Type
type TaggedIR = PolyIR Scheme Type

instance Inferable IRNode TaggedIRNode where
    infer (Var id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        pure (t, Var id)
    infer (Annot e t) = do
        (t0, e') <- infer e
        mono <- ask
        constrain (Rigid t0 t)
        pure (t0, Annot e' t)
    infer (Lam n e) = do
        b <- fresh
        let t0 = Node () (TypeVar b)
        (t1, e') <- inEnv (Set.singleton b) (infer e)
        abstract n t0
        pure (t0 --> t1, Lam n e')
    infer (Let [(n, e0)] e1) = do
        (t0, e0') <- infer e0
        (t1, e1') <- infer e1
        letabst n t0
        pure (t1, Let [(n, e0')] e1')

instance Inferable TypeNode TypeNode where
    infer o@(TypeVar id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume (LocalIdentifier id) t
        pure (t, o)
    infer o@(NamedType id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        pure (t, o)
    infer FunctionType = pure (functionkind, FunctionType)
    infer KindStar = pure (Node () KindStar, KindStar)

instance Inferable i o => Inferable (AppGraph i) (TaggedAppGraph Type o) where
    infer (Node () i) = do
        (t, e) <- infer i
        pure (t, Node t e)
    infer (App () e0 e1) = do
        (t0, e0') <- infer e0
        (t1, e1') <- infer e1
        b <- fresh
        let t2 = Node () (TypeVar b)
        constrain (Unify t0 (App () (App () (Node () FunctionType) t1) t2))
        pure (t2, App t2 e0' e1')