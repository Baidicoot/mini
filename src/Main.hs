module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Type
import Types.Ident
import Types.Graph
import Types.Pretty
import Types.IR

import qualified Data.Map as Map

import Frontend.IRify
import Frontend.GenEnv
import Frontend.Constraint
import Frontend.Solve
import Frontend.CPSify
import Frontend.ClosureConv

import qualified Types.CPS as CPS

a = let (Right exp) = parse (many rpncc) "" "(mul (x y) (* x y))" in exp
b = let (Right exp) = runParse (parsetoplevel a) in exp

typ s = let (Right e) = parse rpncc "" s in let (Right t) = runParse (parsetype e) in t

(enva, envb) = genImportMap . Include $ Namespace ["Prelude"] ["0", "1", "6", "*", "-", "App", "Prod", "Pair", "Just", "Nothing", "Id", "Int", "Bool", "False", "True", "Nested", "s", "k", "w", "fac", "compose", "weird", "mmap", "mul"] ["Int", "Bool", "Pair", "Expr", "Maybe", "Nested"]
int = Node () (NamedType (ExternalIdentifier ["Prelude"] "Int"))
bool = Node () (NamedType (ExternalIdentifier ["Prelude"] "Bool"))
pre = ExternalIdentifier ["Prelude"]
arc = ExternalIdentifier ["Arc"]
gen = generalize mempty
typeenv = Map.fromList [
    (pre "1", gen int),
    (pre "6", gen int),
    (pre "0", gen int),
    (pre "False", gen bool),
    (pre "True", gen bool),
    (pre "*", gen $ int --> int --> int),
    (pre "-", gen $ int --> int --> int),
    (arc "Unit", gen $ typ "Arc.Unit"),
    (pre "Pair", gen $ typ "(a -> b -> (Prelude.Pair a b))"),
    (pre "App", gen $ typ "((a -> b) -> a -> (Prelude.Expr b))"),
    (pre "Prod", gen $ typ "(a -> b -> (Prelude.Expr (Prelude.Pair a b)))"),
    (pre "Bool", gen $ typ "(Prelude.Expr Prelude.Bool)"),
    (pre "Int", gen $ typ "(Prelude.Expr Prelude.Int)"),
    (pre "Id", gen $ typ "(a -> (Prelude.Expr a))"),
    (pre "Just", gen $ typ "(a -> (Prelude.Maybe a))"),
    (pre "Nothing", gen $ typ "(Prelude.Maybe a)"),
    (pre "Nested", gen $ typ "((Prelude.Nested a) -> (Prelude.Nested (Prelude.Nested a)))")]
(Right (ind, x)) = evalIRifier names (enva `Map.union` (Map.singleton (arc "Unit") (arc "Unit")), envb, mempty) (irify b)
cons = Map.fromList (concatMap (\(Ind _ _ s) -> s) ind)
t :: (Type, PolyIR Scheme Type)
(Right (t, n, as, cs)) = runInfer (infer x) typeenv cons names
(typed, tagged) = t

problem = solve cs
Right (subst, ns) = runSolve problem n
y = apply subst tagged

renv = Map.fromList [(pre "Just", (0, 2)), (pre "Nothing", (1, 2))]

(z, ns') = runCPSify ns renv (convert x (\z -> pure CPS.Halt))

funcs = functions z
fmd = functionMeta funcs z
{-
prelude = genImportMap . Include $ Namespace ["Prelude"] ["+", "-", "*", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ["Int"]
localenv = genImportMap (Include $ genNamespace ["Local"] b)

env =
    let (a, b) = prelude
        (x, y) = localenv in
            (a `Map.union` x, b `Map.union` y)
-}
main = pure ()