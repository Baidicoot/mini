module Types.Pattern where

import Types.Ident
import Types.Graph

data PatternNode
    = PatternCons Identifier
    | PatternVar Name
    | PatternWildcard
    deriving(Eq)

instance Show PatternNode where
    show (PatternCons id) = show id
    show (PatternVar n) = n
    show PatternWildcard = "_"

type Pattern = AppGraph PatternNode