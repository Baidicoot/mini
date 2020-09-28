{-# LANGUAGE LambdaCase #-}
module Frontend.Solve where

-- resources:
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/hmf.pdf

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

typenames :: [Name]
typenames = fmap (('t':) . show) [0..]

annotate :: Typespace -> IR -> Either TypeError TaggedIR
annotate (Typespace globals constructors) ir = do
    (ir', ns, _, cs) <- runInfer (infer ir) globals constructors typenames
    subst <- solve cs

selectWithRest :: (a -> [a] -> Bool) -> [a] -> (Maybe a, [a])
selectWithRest = internal []
    where
        internal :: [a] -> (a -> [a] -> Bool) -> [a] -> (Maybe a, [a])
        internal r f (x:xs)
            | f x (r++xs) = (Just x, r++xs)
            | otherwise = internal (r++[x]) f xs
        internal _ _ [] = (Nothing, [])

type Solver = StateT [Name] (Except TypeError)
type Rigid = Set.Set Name

runSolve :: Solver a -> [Name] -> Either TypeError (a, [Name])
runSolve m = runExcept . runStateT m

fresh :: Solver Type
fresh = do
    names <- get
    let (n:ns) = names
    put ns
    pure (Node () (TypeVar n))

instantiateSubst :: Scheme -> Solver (Type, Subst)
instantiateSubst (Forall poly t) = do
    let as = Set.toList poly
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    pure (apply s t, s)

instantiate :: Scheme -> Solver Type
instantiate = fmap fst . instantiateSubst

-- compose substitutions with respect to rigid variables & overlapping bindings
compose :: Rigid -> Subst -> Subst -> Solver Subst
compose rigid s0 s1 =
    let s0' = apply s1 s0
        s1' = apply s0 s1
        key = Set.toList $ Map.keysSet s0 `Set.union` Map.keysSet s1
        (collisions, newsubst) = foldr (\k (c, n) -> case (Map.lookup k s0', Map.lookup k s1') of
            (Just x, Just y) -> ((x, y):c, (k, x):n)
            (Just x, Nothing) -> (c, (k, x):n)
            (Nothing, Just x) -> (c, (k, x):n)
            _ -> undefined) ([], []) key
    in do
        resolutions <- mapM (\(a, b) -> unify rigid a b) collisions
        foldM (\s0 s1 -> compose rigid s0 s1) (Map.fromList newsubst) resolutions

occursCheck :: Name -> Type -> Bool
occursCheck n t = n `Set.member` ftv t

bind :: Rigid -> Name -> Type -> Solver Subst
bind rigid n0 t@(Node () (TypeVar n1))
    | n0 == n1 = pure mempty
    | n0 `Set.member` rigid && n1 `Set.member` rigid = throwError $ RigidityDup n0 n1
    | n0 `Set.member` rigid = pure (Map.singleton n1 (Node () (TypeVar n0)))
    | otherwise = pure (Map.singleton n0 t)
bind rigid n0 t
    | occursCheck n0 t = throwError $ InfiniteType n0 t
    | n0 `Set.member` rigid = throwError $ RigidityFail n0 t
    | otherwise = pure (Map.singleton n0 t)

unify :: Rigid -> Type -> Type -> Solver Subst
unify rigid (Node () (TypeVar n)) t1 = bind rigid n t1
unify rigid t0 (Node () (TypeVar n)) = bind rigid n t0
unify rigid (App () a b) (App () x y) = do
    s0 <- unify rigid a x
    s1 <- unify rigid b y
    compose rigid s0 s1
unify _ a b
    | a == b = pure mempty
    | otherwise = throwError $ UnificationFail a b

infix 9 ~~
(~~) :: Type -> Type -> Solver Subst
(~~) = unify Set.empty

infix 9 @@
(@@) :: Subst -> Subst -> Solver Subst
(@@) = compose Set.empty

activeVars :: Constraint -> Set.Set Name
activeVars (Unify t0 t1) = ftv t0 `Set.union` ftv t1
activeVars (Gen t0 t1 mono _) = ftv t0 `Set.union` (mono `Set.intersection` ftv t1)
activeVars (Inst t0 s0 _) = ftv t0 `Set.union` ftv s0

activevars :: [Constraint] -> Set.Set Name
activevars = foldr Set.union Set.empty . fmap activeVars

solveable :: Constraint -> [Constraint] -> Bool
solveable (Gen _ t1 mono _) c = Set.null $ (ftv t1 `Set.difference` mono) `Set.intersection` activevars c
solveable (Inst t s _) c = Set.null $ ftv s `Set.intersection` activevars c
solveable (Unify _ _) _ = True

solveConstraint :: Constraint -> Solver Subst
solveConstraint (Unify a b) = a ~~ b
solveConstraint (Gen t0 t1 m f) = solveConstraint (Inst t0 (generalize m t1) f)
solveConstraint (Inst t0 sigma Rigid) = do
    (t1, s) <- instantiateSubst sigma
    unify (ftv s) t0 t1
solveConstraint (Inst t0 sigma Wobbly) = do
    t1 <- instantiate sigma
    t0 ~~ t1

solveSingle :: [Constraint] -> Solver ([Constraint], Subst)
solveSingle [] = pure ([], mempty)
solveSingle cs = do
    let (c, cs') = selectWithRest solveable cs
    case c of
        Nothing -> throwError $ Unsolvable cs
        Just c -> do
            s0 <- solveConstraint c
            pure (apply s0 cs', s0)

solve :: [Constraint] -> Solver Subst
solve [] = pure mempty
solve cs = do
    let (c, cs') = selectWithRest solveable cs
    case c of
        Nothing -> throwError $ Unsolvable cs
        Just c -> do
            s0 <- solveConstraint c
            s1 <- solve (apply s0 cs')
            s1 @@ s0