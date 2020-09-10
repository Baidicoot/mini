module Types.Prim where

data UnboxedLit
    = Int Int
    | Unit
    deriving(Eq)

data Primop
    = CCall String
    deriving(Eq, Show)

instance Show UnboxedLit where
    show (Int i) = '#':(show i)