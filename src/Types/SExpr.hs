module Types.SExpr where

import Data.List (intercalate)
import Text.Parsec.Pos

data SExpr a
    = SNode SourcePos a
    | SExpr SourcePos [SExpr a]
    | SRcrd SourcePos [SExpr a]
    | SCmnt SourcePos String
    deriving(Eq)

getPos :: SExpr a -> SourcePos
getPos (SNode p _) = p
getPos (SExpr p _) = p
getPos (SRcrd p _) = p
getPos (SCmnt p _) = p

instance Show a => Show (SExpr a) where
    show (SNode _ a) = show a
    show (SExpr _ xs) = "(" ++ (intercalate " " (map show xs)) ++ ")"
    show (SRcrd _ xs) = "{" ++ (intercalate " " (map show xs)) ++ "}"
    show (SCmnt _ xs) = "(. " ++ xs ++ " .)"

display :: Show a => SExpr a -> String
display s = "'" ++ show s ++ "'"