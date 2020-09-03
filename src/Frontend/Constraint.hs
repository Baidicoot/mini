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
    Infer,
    runInfer,
    infer,
    generalize,
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
    deriving(Eq)

data Constraint
    = Unify Type Type
    | Gen Type Type Monomorphic Flexibility
    | Inst Type Scheme Flexibility
    deriving(Eq)

instance Show Flexibility where
    show Rigid = "ᴿ"
    show Wobbly = "ᵂ"

instance Show Constraint where
    show (Unify t0 t1) = show t0 ++ " ≡ " ++ show t1
    show (Gen t0 t1 m f) = show t0 ++ " ⊑" ++ show f ++ "{" ++ unwords (Set.toList m) ++ "} " ++ show t1
    show (Inst t0 s f) = show t0 ++ " ⊑" ++ show f ++ " " ++ show s

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
    | UnknownPattern Identifier
    | WrongArgs Identifier [Name]
    deriving(Eq, Show)

type InferState = ([Name], AssumptionSet)

type Globals = Map.Map Identifier Scheme

type Infer = RWST
    (Monomorphic, Globals)
    [Constraint]
    InferState
    (Except TypeError)

monomorphic :: Infer Monomorphic
monomorphic = fmap fst ask

globals :: Infer Globals
globals = fmap snd ask

runInfer :: Infer a -> Globals -> [Name] -> Either TypeError (a, [Name], AssumptionSet, [Constraint])
runInfer a g s = (\(a, (s, as), cs) -> (a, s, as, cs)) <$> runExcept (runRWST a (Set.empty, g) (s, []))

fresh :: Infer Name
fresh = do
    (names, as) <- get
    let (n:ns) = names
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

letabst :: Identifier -> Type -> Infer ()
letabst n t = do
    mono <- monomorphic
    (_, as) <- get
    mapM_ (constrain . (\t0 -> Gen t0 t mono Wobbly) . snd) $ filter ((==n) . fst) as

globabst :: Identifier -> Scheme -> Infer ()
globabst id s = do
    (_, as) <- get
    mapM_ (constrain . (\t -> Inst t s Wobbly) . snd) $ filter ((==id) . fst) as

rigidabst :: Name -> Type -> Infer ()
rigidabst n t = do
    mono <- monomorphic
    (_, as) <- get
    mapM_ (constrain . (\t0 -> Gen t0 t mono Rigid) . snd) $ filter ((==(LocalIdentifier n)) . fst) as

constabst :: Identifier -> Scheme -> Infer ()
constabst n s = do
    (_, as) <- get
    mapM_ (constrain . (\t -> Inst t s Rigid) . snd) $ filter ((==n) . fst) as

constrain :: Constraint -> Infer ()
constrain x = tell [x]

inEnv :: (Set.Set Name) -> Infer a -> Infer a
inEnv n = local (\(m, g) -> (Set.union n m, g))

outEnv :: (Set.Set Name) -> Infer a -> Infer a
outEnv n = local (\(m, g) -> (m `Set.difference` n, g))

functionkind :: Kind
functionkind = (Node () KindStar) --> (Node () KindStar) --> (Node () KindStar)

class Inferable i o where
    infer :: i -> Infer (Type, o)

{-
instance Inferable PatternNode (Maybe (Name, Type)) where
    infer PatternWildcard = do
        b <- fresh
        pure (Node () (TypeVar b), Nothing)
    infer (PatternVar n) = do
        b <- fresh
        let id = LocalIdentifier n
        let t = Node () (TypeVar b)
        pure (t, Just (n, t))
    infer (PatternCons id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        glob <- globals
        case Map.lookup id glob of
            Just sch -> constrain (Inst t sch Wobbly)
            Nothing -> throwError $ UnknownPattern id
        pure (t, Nothing)
-}
instance Inferable IRPattern (Map.Map Name Type) where
    infer IRWild = do
        b <- fresh
        pure (Node () (TypeVar b), Map.empty)
    infer (IRCons id args) = do
        glob <- globals
        case Map.lookup id glob of
            Just sch@(Forall _ scht) ->
                if arity scht /= length args then
                    throwError $ WrongArgs id args
                else do
                    b <- fresh
                    let t = Node () (TypeVar b)
                    argtvars <- mapM (\n -> do
                        b <- fresh
                        let v = LocalIdentifier n
                        let t = Node () (TypeVar b)
                        assume v t
                        pure (n, t)) args
                    let it = foldr (-->) t (fmap snd argtvars)
                    constrain (Inst it sch Wobbly)
                    pure (t, Map.fromList argtvars)
            Nothing -> throwError $ UnknownPattern id

type TaggedIRNode = PolyIRNode Scheme Type
type TaggedIR = PolyIR Scheme Type

instance Inferable IRNode TaggedIRNode where
    infer (Var id) = do
        b <- fresh
        let t = Node () (TypeVar b)
        assume id t
        glob <- globals
        case Map.lookup id glob of
            Just sch -> constrain (Inst t sch Wobbly)
            Nothing -> pure ()
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
    infer (Fix ds e) = do
            (ds', lp) <- foldM (\(ds', lp) (n, e) -> do
                (t, e') <- infer e
                pure ((n, e'):ds', (n, t):lp)) ([], []) ds
            (t, e') <- infer e
            mapM_ (uncurry letabst) lp
            pure (t, Fix (reverse ds') e')
    infer (Let n x e) = do
        (xt, x') <- infer x
        letabst (LocalIdentifier n) xt
        (t, e') <- infer e
        pure (t, Let n x' e')
    infer (Match e ps) = do
        b <- fresh
        let et = Node () (TypeVar b)
        mono <- monomorphic
        (it, ot, cases) <- foldM (\(its, ots, cases) (p, e) -> do
            (pt, vartypes) <- infer p
            let names = Map.toList (vartypes :: Map.Map Name Type)
            (et, e') <- inEnv (Set.fromList $ fmap ((\(Node () (TypeVar n)) -> n) . snd) names) (infer e)
            mapM_ (uncurry rigidabst) names
            pure (pt:its, et:ots, (p, e'):cases)) ([], [], []) ps
        mapM_ (\it -> constrain (Unify et it)) it
        b <- fresh
        let t = Node () (TypeVar b)
        mapM_ (\ot -> constrain (Unify t ot)) ot
        pure (t, Match e (reverse cases))

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