module Types.Pattern where

import Types.Ident

data Pattern
    = PatternCons Identifier
    | PatternVar Name
    | PatternWildcard
    | PatternApp Pattern Pattern
    deriving(Eq, Show)