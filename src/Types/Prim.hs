module Types.Prim where
import Types.Ident

data UnboxedLit
    = Int Int
    | Char Char
    deriving(Eq, Ord)

data LitType
    = IntTy
    | CharTy
    | RefTy
    deriving(Eq, Ord)

instance Show LitType where
    show IntTy = "Int"
    show CharTy = "Char"
    show RefTy = "Ref"

data Primop
    = AAdd
    | ASub
    | ADiv
    | AMul
    | PutChr
    | PutInt
    | CharToInt
    | IntToChar
    | CmpInt
    | CmpChar
    | EqInt
    | EqChar
    | SetRef
    | NewRef
    | GetRef
    deriving(Eq, Ord)

data Value
    = Var Identifier
    | Label Identifier
    | Lit UnboxedLit
    deriving(Eq, Ord)

instance Show Value where
    show (Var id) = show id
    show (Label id) = '<':show id++">"
    show (Lit u) = show u

effectOp :: Primop -> Bool
effectOp PutChr = True
effectOp PutInt = True
effectOp SetRef = True
effectOp _ = False

dataOp :: Primop -> Bool
dataOp IntToChar = True
dataOp CharToInt = True
dataOp AAdd = True
dataOp ASub = True
dataOp ADiv = True
dataOp AMul = True
dataOp NewRef = True
dataOp GetRef = True
dataOp _ = False

switchOp :: Primop -> Bool
switchOp CmpInt = True
switchOp CmpChar = True
switchOp EqInt = True
switchOp EqChar = True
switchOp _ = False

branchesOp :: Primop -> Maybe Int
branchesOp CmpInt = Just 3
branchesOp CmpChar = Just 3
branchesOp EqInt = Just 2
branchesOp EqChar = Just 2
branchesOp _ = Nothing

arityOp :: Primop -> Int
arityOp AAdd = 2
arityOp ASub = 2
arityOp ADiv = 2
arityOp AMul = 2
arityOp PutChr = 1
arityOp PutInt = 1
arityOp IntToChar = 1
arityOp CharToInt = 1
arityOp CmpInt = 5
arityOp CmpChar = 5
arityOp EqInt = 4
arityOp EqChar = 4
arityOp SetRef = 2
arityOp NewRef = 1
arityOp GetRef = 1

cmpOp :: LitType -> Primop
cmpOp IntTy = CmpInt
cmpOp CharTy = CmpChar

eqOp :: LitType -> Primop
eqOp IntTy = EqInt
eqOp CharTy = EqChar

instance Show Primop where
    show AAdd = "#+"
    show ASub = "#-"
    show AMul = "#*"
    show ADiv = "#/"
    show PutChr = "#putchr"
    show PutInt = "#putint"
    show IntToChar = "#chr"
    show CharToInt = "#ord"
    show CmpInt = "#cmpint"
    show CmpChar = "#cmpchr"
    show EqInt = "#eqint"
    show EqChar = "#eqchr"
    show SetRef = "#setref"
    show NewRef = "#newref"
    show GetRef = "#getref"

instance Show UnboxedLit where
    show (Int i) = '$':show i
    show (Char c) = '\'':c:"'"