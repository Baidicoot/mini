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

data Emit
    = EmitLit UnboxedLit
    | EmitLabel Label Int
    deriving(Eq)

instance Show Emit where
    show (EmitLit l) = show l
    show (EmitLabel l 0) = '%':show l
    show (EmitLabel l o) = '%':show l ++ " + " ++ show o

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
    = Define Label
    | Comment String
    | Exports Label
    | Imports Label
    | Table Label [Emit]
    -- end of pseudoops
    | CheckLim Int
    | Jmp Operand
    | Record [(Operand,AccessPath)] Register
    | Free Register
    | Select Int Operand Register
    | Fetch Register Operand Operand
    | Move Register Operand
    | Halt
    | Error String
    deriving(Eq)

instance Show Operator where
    show (Imports l) = "imports " ++ show l
    show (Exports l) = "exports " ++ show l
    show (Define l) = '@':show l ++ ":"
    show (CheckLim i) = "check " ++ show i
    show (Jmp o) = "jmp " ++ show o
    show (Record p r) = "record " ++ concatMap (\(o,p) -> show o ++ show p ++ ", ") p ++ show r
    show (Free r) = "free " ++ show r
    show (Select i o r) = "select " ++ show i ++ ", " ++ show o ++ ", " ++ show r
    show (Fetch r o0 o1) = "fetch " ++ show r ++ ", " ++ show o0 ++ ", " ++ show o1
    show (Move a b) = "move " ++ show a ++ ", " ++ show b
    show Halt = "halt"
    show (Error str) = "error \"" ++ str ++ "\""
    show (Comment str) = "(* " ++ str ++ " *)"
    show (Table l xs) = "table " ++ show l ++ " " ++ concatMap ((++",") . show) xs 

    showList xs s = concatMap (\o -> show o ++ "\n") xs ++ s