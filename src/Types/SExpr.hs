module Types.SExpr where

import Data.List (intercalate)

data SExpr a
    = SNode a
    | SExpr [SExpr a]
    deriving(Eq)

instance Show a => Show (SExpr a) where
    show (SNode a) = show a
    show (SExpr xs) = "(" ++ (intercalate " " (map show xs)) ++ ")"