module Types.Literal where

data UnboxedLit
    = Int Int
    deriving(Eq)

instance Show UnboxedLit where
    show (Int i) = '#':(show i)