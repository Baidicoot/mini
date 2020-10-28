module Types.SExpr where

import Data.List (intercalate)
import Text.Parsec.Pos

data SExpr a
    = SNode SourcePos a
    | SExpr SourcePos [SExpr a]
    deriving(Eq)

getPos :: SExpr a -> SourcePos
getPos (SNode p _) = p
getPos (SExpr p _) = p

instance Show a => Show (SExpr a) where
    show (SNode _ a) = show a
    show (SExpr _ xs) = "(" ++ (intercalate " " (map show xs)) ++ ")"

display :: Show a => SExpr a -> String
display s = "'" ++ show s ++ "'"