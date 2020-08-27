module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Syntax
import Types.Type
import Types.Ident
import Types.Graph
import Types.Pretty

import qualified Data.Map as Map

import Frontend.IRify
import Frontend.GenEnv
import Frontend.Constraint
import Frontend.Solve

a = let (Right exp) = parse rpncc "" "(let (test (x) (match x ((Pair _ (Pair (Just a) b)) (a b)) ((Pair (Pair (Just Nothing) (Pair Nothing b)) Nothing) x))) test)" in exp
b = let (Right exp) = runParse (parseexpr a) in exp

typ s = let (Right e) = parse rpncc "" s in let (Right t) = runParse (parsetype e) in t

env = genImportMap . Include $ Namespace ["Prelude"] ["0", "1", "6", "*", "-", "App", "Prod", "Pair", "Just", "Nothing", "Id", "Int", "Bool", "False", "True"] ["Int", "Bool", "Pair", "Expr", "Maybe"]
int = Node () (NamedType (ExternalIdentifier ["Prelude"] "Int"))
bool = Node () (NamedType (ExternalIdentifier ["Prelude"] "Bool"))
pre = ExternalIdentifier ["Prelude"]
gen = generalize mempty
typeenv = Map.fromList [
    (pre "1", gen int),
    (pre "6", gen int),
    (pre "0", gen int),
    (pre "False", gen bool),
    (pre "True", gen bool),
    (pre "*", gen $ int --> int --> int),
    (pre "-", gen $ int --> int --> int),
    (pre "Pair", gen $ typ "(a -> b -> (Prelude.Pair a b))"),
    (pre "App", gen $ typ "((a -> b) -> a -> (Prelude.Expr b))"),
    (pre "Prod", gen $ typ "(a -> b -> (Prelude.Expr (Prelude.Pair a b)))"),
    (pre "Bool", gen $ typ "(Prelude.Expr Prelude.Bool)"),
    (pre "Int", gen $ typ "(Prelude.Expr Prelude.Int)"),
    (pre "Id", gen $ typ "(a -> (Prelude.Expr a))"),
    (pre "Just", gen $ typ "(a -> (Prelude.Maybe a))"),
    (pre "Nothing", gen $ typ "(Prelude.Maybe a)")]
(Right x) = evalIRifier names env (irifyExpr b)
t :: (Type, TaggedAppGraph Type TaggedIRNode)
(Right (t, n, as, cs)) = runInfer (infer x) typeenv names
(typed, tagged) = t

problem = solve cs
Right (subst, ns) = runSolve problem n
y = apply subst tagged
{-
prelude = genImportMap . Include $ Namespace ["Prelude"] ["+", "-", "*", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ["Int"]
localenv = genImportMap (Include $ genNamespace ["Local"] b)

env =
    let (a, b) = prelude
        (x, y) = localenv in
            (a `Map.union` x, b `Map.union` y)
-}
main = pure ()