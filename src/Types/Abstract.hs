module Types.Abstract where
import Types.Prim
import Types.Ident
import Types.CPS (AccessPath)

type GPR = Int

data Register
    = DataPtr
    | DataLim
    | Arith
    | GPR GPR
    deriving(Eq)

class Reg r where
    r :: Int -> r
    dp :: r
    dl :: r
    ar :: r

instance Reg Register where
    r = GPR
    dp = DataPtr
    dl = DataLim
    ar = Arith

instance Reg Operand where
    r = Reg . GPR
    dp = Reg DataPtr
    dl = Reg DataLim
    ar = Reg Arith

instance Show Register where
    show DataPtr = "dp"
    show DataLim = "dl"
    show (GPR i) = "r"++show i
    show Arith = "ar"

type Label = Identifier

data Operand
    = Reg Register
    | ImmLabel Label
    | ImmLit UnboxedLit
    deriving(Eq)

instance Show Operand where
    show (Reg r) = show r
    show (ImmLabel l) = '%':show l
    show (ImmLit l) = show l

data Operator
    = EmitLit UnboxedLit
    | EmitPtr Label Int
    | Define Label
    -- end of pseudoops
    | CheckLim Int
    | Jmp Operand
    | Record [(Operand,AccessPath)] Register
    | Select Int Operand Register
    | Move Register Operand
    deriving(Eq)

instance Show Operator where
    show (EmitLit l) = show l
    show (EmitPtr p o) = ('%':show p) ++ "[" ++ show o ++ "]"
    show (Define l) = '@':show l ++ ":"
    show (CheckLim i) = "check " ++ show i
    show (Jmp o) = "jmp " ++ show o
    show (Record p r) = "record " ++ concatMap (\(o,p) -> show o ++ show p ++ ", ") p ++ show r
    show (Select i o r) = "select " ++ show i ++ ", " ++ show o ++ ", " ++ show r

    showList xs s = concatMap (\o -> show o ++ "\n") xs