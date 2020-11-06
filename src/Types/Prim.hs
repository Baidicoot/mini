module Types.Prim where

data UnboxedLit
    = Int Int
    | Char Char
    | Unit
    deriving(Eq, Ord)

data LitType
    = IntTy
    | CharTy
    | UnitTy
    deriving(Eq, Show, Ord)

data Primop
    = AAdd
    | ASub
    | ADiv
    | AMul
    deriving(Eq, Ord)

arityOp :: Primop -> Int
arityOp AAdd = 2
arityOp ASub = 2
arityOp ADiv = 2
arityOp AMul = 2

litPrimTy :: UnboxedLit -> LitType
litPrimTy (Int _) = IntTy
litPrimTy (Char _) = CharTy
litPrimTy Unit = UnitTy

instance Show Primop where
    show AAdd = "#+"
    show ASub = "#-"
    show AMul = "#*"
    show ADiv = "#/"

instance Show UnboxedLit where
    show (Int i) = '#':(show i)
    show Unit = "()"