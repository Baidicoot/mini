module Types.Prim where

data UnboxedLit
    = Int Int
    | Char Char
    | Unit
    deriving(Eq)

data LitType
    = IntTy
    | CharTy
    | UnitTy
    deriving(Eq, Show)

data Primop
    = AAdd
    | ASub
    | ADiv
    | AMul
    deriving(Eq)

instance Show Primop where
    show AAdd = "#+"
    show ASub = "#-"
    show AMul = "#*"
    show ADiv = "#/"

instance Show UnboxedLit where
    show (Int i) = '#':(show i)
    show Unit = "()"