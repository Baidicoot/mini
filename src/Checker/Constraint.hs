module Checker.Constraint where
-- Constraint - constraint solving

import Types.Kind
import Types.Type (Type(..), TypeError(..))
import Types.Ident
import qualified Data.Map.Strict as Map
import qualified Checker.Kinds as Kinds

import Data.Monoid
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Set as Set

-- from here on adapted from WYAH constraint solving (http://dev.stephendiehl.com/fun/006_hindley_milner.html)

type Solve = Except TypeError

compose :: Subst -> Subst -> Subst
compose a b = fmap (substitute a) b `Map.union` a

bind :: Name -> Type -> Solve Subst
bind n t
    | TypeVar n == t    = pure mempty
    | n `occursIn` t    = throwError (InfiniteType n t)
    | otherwise         = pure (Map.singleton n t)

infix 9 ~~
(~~) :: Type -> Type -> Solve Subst
a ~~ b | a == b = pure mempty
(TypeVar n) ~~ b = bind n b
a ~~ (TypeVar n) = bind n a
(TypeApp a b) ~~ (TypeApp x y) = do
    s0 <- a ~~ x
    s1 <- b ~~ y
    pure (compose s0 s1)
a ~~ b = throwError (UnificationFail a b)

uniMany :: [Type] -> [Type] -> Solve Subst
uniMany [] [] = pure mempty
uniMany (x:xs) (y:ys) = do
    s0 <- x ~~ y
    s1 <- xs `uniMany` ys
    pure (s1 `compose` s0)
uniMany a b = throwError (UnificationMismatch a b)

solver :: Subst -> [Constraint] -> Solve Subst
solver su0 cs = case cs of
    [] -> pure su0
    ((t1, t2):css) -> do
        su1 <- t1 ~~ t2
        solver (su1 `compose` su0) (map (\(a, b) -> (substitute su1 a, substitute su1 b)) css)

runSolve :: Solve x -> Either TypeError x
runSolve = runExcept