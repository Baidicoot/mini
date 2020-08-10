module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Syntax
import Types.Type
import Types.Ident
import Types.Graph

import qualified Data.Map as Map

import Frontend.IRify
import Frontend.GenEnv
import Frontend.Constraint
import Frontend.Solve

a = let (Right exp) = parse rpncc "" "(lam (m) (let (y () m) (let (x () (y 0)) x)))" in exp
b = let (Right exp) = runParse (parseexpr a) in exp

env = genImportMap . Include $ Namespace ["Prelude"] ["0"] ["Int"]
(Right x) = evalIRifier names env (irifyExpr b)
t :: (Type, TaggedAppGraph Type TaggedIRNode)
(Right (t, n, as, cs)) = runInfer (infer x) names

(Right ((s0, cs0), n0)) = runSolver (solveSingle cs) n
(Right ((s1, cs1), n1)) = runSolver (solveSingle cs0) n0
(Right ((s2, cs2), n2)) = runSolver (solveSingle cs1) n1
(Right ((s3, cs3), n3)) = runSolver (solveSingle cs2) n2
{-
prelude = genImportMap . Include $ Namespace ["Prelude"] ["+", "-", "*", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ["Int"]
localenv = genImportMap (Include $ genNamespace ["Local"] b)

env =
    let (a, b) = prelude
        (x, y) = localenv in
            (a `Map.union` x, b `Map.union` y)
-}
main = pure ()