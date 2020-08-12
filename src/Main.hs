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

a = let (Right exp) = parse rpncc "" "(:: (lam (x y) y) (x -> x))" in exp
b = let (Right exp) = runParse (parseexpr a) in exp

typ s = let (Right e) = parse rpncc "" s in let (Right t) = runParse (parsetype e) in t

env = genImportMap . Include $ Namespace ["Prelude"] ["0", "1", "6", "*", "-"] ["Int", "Bool"]
int = Node () (NamedType (ExternalIdentifier ["Prelude"] "Int"))
pre = ExternalIdentifier ["Prelude"]
gen = generalize mempty
typeenv = Map.fromList [(pre "1", gen int), (pre "6", gen int), (pre "0", gen int), (pre "*", gen $ int --> int --> int), (pre "-", gen $ int --> int --> int)]
(Right x) = evalIRifier names env (irifyExpr b)
t :: (Type, TaggedAppGraph Type TaggedIRNode)
(Right (t, n, as, cs)) = runInfer (globalEnv typeenv . infer $ x) names
{-
prelude = genImportMap . Include $ Namespace ["Prelude"] ["+", "-", "*", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ["Int"]
localenv = genImportMap (Include $ genNamespace ["Local"] b)

env =
    let (a, b) = prelude
        (x, y) = localenv in
            (a `Map.union` x, b `Map.union` y)
-}
main = pure ()