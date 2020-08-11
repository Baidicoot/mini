{-# LANGUAGE LambdaCase #-}
module Frontend.Solve where

-- derived from: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf

import Frontend.Constraint
import Types.Graph
import Types.Type
import Types.Pattern
import Types.Ident
import Types.IR

import Data.Maybe
import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

compose :: Subst -> Subst -> Subst
s0 `compose` s1 = Map.map (apply s0) s1 `Map.union` s0

type Solver = StateT [Name] (Except TypeError)

runSolver :: Solver a -> [Name] -> Either TypeError (a, [Name])
runSolver a s = runExcept $ runStateT a s

fresh :: Solver Type
fresh = do
    (n:ns) <- get
    put ns
    pure (Node () (TypeVar n))

instantiate :: Scheme -> Solver Type
instantiate (Forall fv t) = do
    let as = Set.toList fv
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    pure (apply s t)

occursCheck :: Name -> Type -> Bool
occursCheck n = Set.member n . ftv

bind ::  Name -> Type -> Solver Subst
bind a t
    | t == (Node () (TypeVar a)) = pure mempty
    | occursCheck a t = throwError $ InfiniteType a t
    | otherwise = pure (Map.singleton a t)

mgu :: Type -> Type -> Solver Subst
mgu t0 t1 | t0 == t1 = pure mempty
mgu (Node () (TypeVar n)) t1 = bind n t1
mgu t0 (Node () (TypeVar n)) = bind n t0
mgu (App () t0 t1) (App () t2 t3) = do
    s0 <- mgu t0 t2
    s1 <- mgu t1 t3
    pure (s0 `compose` s1)
mgu t0 t1 = throwError $ UnificationFail t0 t1

{- subsumption checks for rank-1 types
for example:
I -> a  <   forall a. a -> a
a -> a  >=  forall a. a -> a
a -> b  >=  forall a. a -> a
a -> a  <   forall a b. a -> b

the rules seem to be:
- each type variable in τ may bind to *only one* polymorphic variable in σ.
- a polymorphic variable in σ may be bound to anything else.
-}

subsUnify :: Type -> Scheme -> Solver Subst
subsUnify (Node () (TypeVar n)) (Forall fv t) = bind n t
subsUnify t (Forall fv r@(Node () (TypeVar n)))
    | n `Set.member` fv =
        if (\case
            Node () (TypeVar _) -> True
            _ -> False) t then
            pure mempty
        else throwError $ RigidityFail t r fv
    | otherwise = pure mempty
subsUnify t0@(App () a b) (Forall poly t1@(App () x y)) = do
    s0 <- subsUnify a (Forall poly x)
    s1 <- subsUnify b (Forall poly y)
    let keyList = Set.toList $ Map.keysSet s0 `Set.union` Map.keysSet s1
    mapM_ (\k ->
        if (length . filter (flip elem poly) . Set.toList . ftv . catMaybes $ [Map.lookup k s0, Map.lookup k s1]) > 1 then
            throwError $ RigidityFail t0 t1 poly
        else
            pure ()) keyList
    pure (s0 `compose` s1)
subsUnify a (Forall _ b)
    | a == b = pure mempty
    | otherwise = throwError $ UnificationFail a b

subsCheck :: Type -> Scheme -> Solver ()
subsCheck t0 s0 = subsUnify t0 s0 >> pure ()

solvePass :: [Constraint] -> Solver (Subst, [Constraint])
solvePass [] = pure (mempty, [])
solvePass ((Unify t0 t1):c) = do
    s <- mgu t0 t1
    (sc, r) <- solvePass (apply s c)
    pure (sc `compose` s, r)
solvePass (i@(Gen t0 t1 m):c)
    | Set.null $ (ftv t1 `Set.difference` m) `Set.intersection` ftv c =
        solvePass ((Inst t0 $ generalize m t1):c)
    | otherwise = do
        (s, sc) <- solvePass c
        pure (s, (apply s i):sc)
solvePass ((Inst t0 sigma):c) = do
    t1 <- instantiate sigma
    solvePass ((Unify t0 t1):c)
solvePass (i@(Subs t0 sigma):c)
    | Set.null $ (ftv t0 `Set.union` ftv sigma) `Set.intersection` ftv c = do
        subsCheck t0 sigma
        t1 <- instantiate sigma
        s <- mgu t0 t1
        (sc, r) <- solvePass (apply s c)
        pure (sc `compose` s, r)
    | otherwise = do
        (s, sc) <- solvePass c
        pure (s, (apply s i):sc)

solve :: [Constraint] -> Solver Subst
solve [] = pure mempty
solve cs = do
    (s0, cs') <- solvePass cs
    if length cs == length cs' then
        throwError $ Ambigious cs
    else case cs' of
        [] -> pure s0
        _ -> do
            s1 <- solve cs'
            pure (s1 `compose` s0)