module Types.IR where

import Types.Ident
import Types.Graph
import Types.Pattern
import Types.Type
import Data.List (intercalate)

data UnboxedLit
    = Int Int
    deriving(Eq)

instance Show UnboxedLit where
    show (Int i) = '#':(show i)

data IRNode
    = Let [(Name, IR)] IR
    | Annot IR Type
    | Lam Name IR
    | Var Identifier
    | Unboxed UnboxedLit
    | Match IR [(Pattern, IR)]
    deriving(Eq)

instance Show IRNode where
    show (Let ds ir) = "let " ++ intercalate "\n    " (fmap (\(n, ir) -> n ++ " = " ++ show ir) ds) ++ " in\n" ++ show ir
    show (Annot ir ty) = "(" ++ show ir ++ " :: " ++ show ty ++ ")"
    show (Lam n ir) = "(\\" ++ n ++ ". " ++ show ir ++ ")"
    show (Var id) = show id
    show (Unboxed l) = show l
    show (Match ir cases) = "match " ++ show ir ++ unwords (fmap (\(p, ir) -> "(" ++ show p ++ " -> " ++ show ir ++ ")") cases)

type IR = AppGraph IRNode