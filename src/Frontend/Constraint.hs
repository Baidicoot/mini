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
    Flexibility(..),
    runInfer,
    infer,
    generalize,
    globalEnv,
    (-->)
) where

-- derived from: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf

import Frontend.GenEnv
import Types.Graph
import Types.Type
import Types.Pattern
import Types.Ident
import Types.IR

import Data.Monoid
import Data.Foldable
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate)

data Flexibility
    = Rigid
    | Wobbly
    deriving(Eq, Show)

data Constraint
    = Unify Type Type
    | Gen Type Type Monomorphic Flexibility
    | Inst Type Scheme Flexibility
    deriving(Eq)

instance Show Constraint where
    show (Unify t0 t1) = show t0 ++ " ≡ " ++ show t1
    show (Gen t0 t1 m _) = show t0 ++ " ⊑{" ++ unwords (Set.toList m) ++ "} " ++ show t1
    show (Inst t0 s _) = show t0 ++ " ⊑ " ++ show s

type AssumptionSet = [(Identifier, Type)]

instance Substitutable Constraint where
    apply s (Unify t0 t1) = Unify (apply s t0) (apply s t1)
    apply s (Gen t0 t1 m f) = Gen (apply s t0) (apply s t1) sm f
        where
            sm = Set.unions . Set.map (\n -> ftv $ Map.findWithDefault (Node () (TypeVar n)) n s) $ m
    apply s (Inst tau sigma f) = Inst (apply s tau) (apply s sigma) f

    ftv (Unify t0 t1) = ftv t0 `Set.union` ftv t1
    ftv (Gen t0 t1 m _) = ftv t0 `Set.union` (ftv t1 `Set.intersection` m)
    ftv (Inst tau sigma _) = ftv tau `Set.union` ftv sigma

data TypeError
    = UnificationFail Type Type
    | InfiniteType Name Type
    | Unsolvable [Constraint]
    | RigidityFail Name Type
    | RigidityDup Name Name
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
    mapM_ (constrain . (\t0 -> Gen t0 t mono Wobbly) . snd) $ filter ((==(LocalIdentifier n)) . fst) as

rigidabst :: Name -> Type -> Infer ()
rigidabst n t = do
    mono <- ask
    (_, as) <- get
    mapM_ (constrain . (\t0 -> Gen t0 t mono Rigid) . snd) $ filter ((==(LocalIdentifier n)) . fst) as

constabst :: Identifier -> Scheme -> Infer ()
constabst n s = do
    (_, as) <- get
    mapM_ (constrain . (\t -> Inst t s Rigid) . snd) $ filter ((==n) . fst) as

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

instance Inferable PatternNode (Maybe (Name, Type)) where
    infer PatternWildcard = do
        b <- fresh
        pure (Node () (TypeVar b), Nothing)
    infer (PatternVar n) = do
        b <- fresh
        let id = LocalIdentifier n
        let t = Node () (TypeVar b)
        assume id t
        pure (t, Just (n, t))
    infer (PatternCons id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        pure (t, Nothing)

type TaggedIRNode = PolyIRNode Scheme Type
type TaggedIR = PolyIR Scheme Type

instance Inferable IRNode TaggedIRNode where
    infer (Var id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        pure (t, Var id)
    infer (Annot e s) = do
        (t0, e') <- infer e
        constrain (Inst t0 s Rigid)
        pure (t0, Annot e' s)
    infer (Lam n e) = do
        b <- fresh
        let t0 = Node () (TypeVar b)
        (t1, e') <- inEnv (Set.singleton b) (infer e)
        abstract n t0
        pure (t0 --> t1, Lam n e')
    infer (Let ds e) = do
        (ds', lp) <- foldM (\(ds', lp) (n, e) -> do
            (t, e') <- infer e
            pure ((n, e'):ds', (n, t):lp)) ([], []) ds
        (t, e') <- infer e
        mapM_ (uncurry letabst) lp
        pure (t, Let ds' e')
    infer (Match e ps) = do
        (et, e') <- infer e
        mono <- ask
        (it, ot, cases) <- foldM (\(its, ots, cases) (p, e) -> do
            (pt, taggedGraph) <- infer p
            constrain (Gen pt et mono Wobbly)
            let names = catMaybes $ toList taggedGraph
            (et, e') <- inEnv (Set.fromList $ fmap fst names) infer e
            mapM_ (uncurry rigidabst) names
            pure (pt:its, et:ots, (p, e'):cases)) ([], []) ps
        mapM_ (\it -> constrain (Gen it et mono Wobbly)) it
        b <- fresh
        let t = Node () (TypeVar b)
        mapM_ (\ot -> constrain (Unify ot t)) ot
        pure (Match e' cases, t)

    {-
    infer (Match e ps) = do
        _ <- mapM (\(p, e) -> do
            )
    
    infer (Let [(n, e0)] e1) = do
        (t0, e0') <- infer e0
        (t1, e1') <- infer e1
        letabst n t0
        pure (t1, Let [(n, e0')] e1')
    -}

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

globalEnv :: Map.Map Identifier Scheme -> Infer a -> Infer a
globalEnv env i = do
    a <- i
    mapM_ (\(n, s) -> constabst n s) (Map.toList env)
    pure a