module Types.Prim where
import Types.Ident

data UnboxedLit
    = Int Int
    | Char Char
    deriving(Eq, Ord)

data LitType
    = IntTy
    | CharTy
    deriving(Eq, Ord)

instance Show LitType where
    show IntTy = "Int"
    show CharTy = "Char"

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

arithOp :: Primop -> Bool
arithOp AAdd = True
arithOp ASub = True
arithOp ADiv = True
arithOp AMul = True
arithOp _ = False

effectOp :: Primop -> Bool
effectOp PutChr = True
effectOp PutInt = True
effectOp _ = False

coerceOp :: Primop -> Bool
coerceOp IntToChar = True
coerceOp CharToInt = True
coerceOp _ = False

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

litPrimTy :: UnboxedLit -> LitType
litPrimTy (Int _) = IntTy
litPrimTy (Char _) = CharTy

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

instance Show UnboxedLit where
    show (Int i) = '$':show i
    show (Char c) = '\'':c:"'"