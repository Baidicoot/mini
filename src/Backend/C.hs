{-# LANGUAGE LambdaCase #-}

module Backend.C where

import Types.Abstract
import Types.CPS (AccessPath(..))
import Types.Ident
import qualified Types.Prim as Prim
import Data.List

import Control.Monad.IO.Class

import Types.Build
import Types.Prim (Primop(..))

import Control.Arrow

import System.Directory (copyFile)

sanitize :: String -> String
sanitize = concatMap (\case
    '=' -> "_eq_"
    '|' -> "_pipe_"
    '&' -> "_amp_"
    '_' -> "_under_"
    '>' -> "_gt_"
    '<' -> "_lt_"
    c -> [c])

mangle :: ModulePath -> Label -> String
mangle p (LocalIdentifier l) = concatMap (++"_") p ++ sanitize l
mangle p (ExternalIdentifier m l) = concatMap (++"_") m ++ sanitize l

data Offset
    = POff AccessPath
    | OOff Operand

showLit :: Prim.UnboxedLit -> String
showLit (Prim.Char c) = '\'':c:'\'':""
showLit (Prim.Int i) = show i

showOp :: Bool -> ModulePath -> Operand -> String
showOp b p (Reg Arith) = "reg_arith"
showOp b p (Reg DataPtr) = "data_ptr"
showOp b p (Reg DataLim) = "data_lim"
showOp b p (Reg (GPR n)) = "reg_gpr[" ++ show n ++ "]"
showOp b p (ImmLabel l) = (if b then "&&" else "") ++ mangle p l
showOp b p (ImmLit l) = showLit l

showArith :: Primop -> String
showArith AAdd = " + "
showArith ASub = " - "
showArith AMul = " * "
showArith ADiv = " / "

translateOp :: Bool -> ModulePath -> Operand -> Offset -> String
translateOp b mp op (POff p) = showPath p
    where
        showPath (SelPath i x) = "((void**)(" ++ showPath x ++ "))[" ++ show i ++ "]"
        showPath NoPath = showOp b mp op
translateOp b mp op1 (OOff op2) = "((void**)(" ++ showOp b mp op1 ++ "))[(long int)" ++ showOp b mp op2 ++ "]"

translate :: Bool -> ModulePath -> Operator -> String
translate d p (Table t xs) = "void* " ++ mangle p t ++ "[] = {"  ++ intercalate "," (fmap (\case
    ImmLit l -> showLit l
    ImmLabel l -> "&&" ++ mangle p l) xs) ++ "};\n"
translate d p (Define l) = mangle p l ++ ": ;\n" ++ if d then "printf(\"entered " ++ mangle p l ++ "\\n\");\n" else ""
translate d p (Comment s) = if d then  "/* " ++ s ++ " */\n" else ""
translate d p (Jmp (ImmLabel l)) = "goto " ++ mangle p l ++ ";\n"
translate d p (Jmp o) = "reg_arith = " ++ showOp True p o ++ ";\n"
    ++ (if d then "printf(\"jumping to %x\\n\",reg_arith);\n" else "")
    ++ "goto *reg_arith" ++ ";\n"
translate d p (Record ps r) = showOp True p (Reg r) ++ " = alloc_in_arena(heap, sizeof(void*)*" ++ show (length ps) ++ ");\n"
    ++ concatMap (\((o,pa),i)->translateOp True p (Reg r) (POff $ SelPath i NoPath) ++ " = " ++ translateOp True p o (POff pa) ++ ";\n") (zip ps [0..])
    ++ if d then concatMap (\(_,i) -> "printf(\"%x, \"," ++ translateOp True p (Reg r) (POff $ SelPath i NoPath) ++ ");") (zip ps [0..]) ++ "printf(\"\\n\");\n" else ""
translate d p (Select i o r) =
    (if d then "printf(\"selected %x from %x["++ show i ++"]\\n\"," ++ translateOp True p o (POff $ SelPath i NoPath) ++ "," ++ translateOp True p o (POff NoPath) ++ ");\n" else "")
    ++ showOp True p (Reg r) ++ " = " ++ translateOp True p o (POff $ SelPath i NoPath) ++ ";\n"
translate d p (Fetch r o1 o2) =
    showOp True p (Reg r) ++ " = " ++ translateOp False p o1 (OOff o2) ++ ";\n"
    ++ if d then "printf(\"%x\\n\", " ++ showOp True p (Reg r) ++ ");\n" else ""
translate d p Halt = "free_arena(heap);\nreturn 0;\n"
translate d p (Error s) = "printf(" ++ show (s++"\n") ++ ");\nfree_arena(heap);\nreturn 1;\n"
translate d p (Move r op) = showOp True p (Reg r) ++ " = " ++ showOp True p op ++ ";\n"
translate d _ (Exports _) = ""
translate d _ (Imports _) = ""
translate d p (EffectOp PutChr o) = "putchar(" ++ showOp True p o ++ ");\n"
translate d p (EffectOp PutInt o) = "printf(\"%i\"," ++ showOp True p o ++ ");\n"
translate d p (CoerceOp IntToChar r o) = showOp True p (Reg r) ++ " = (char)" ++ showOp True p o ++ ";\n"
translate d p (CoerceOp CharToInt r o) = showOp True p (Reg r) ++ " = (long int)" ++ showOp True p o ++ ";\n"
translate d p (SwitchOp op r [o1,o2] [eq,gt,lt]) | op == CmpInt || op == CmpChar =
    showOp True p (Reg r) ++ " = " ++ showOp True p o1 ++ " == " ++ showOp True p o2 ++ " ? "
    ++ showOp True p eq ++ " : (" ++
        showOp True p o1 ++ " > " ++ showOp True p o2 ++ " ? " ++ showOp True p gt ++ " : " ++ showOp True p lt
    ++ ");\n"
translate d p (SwitchOp op r [o1,o2] [eq,ne]) | op == EqInt || op == EqChar =
    showOp True p (Reg r) ++ " = " ++ showOp True p o1 ++ " == " ++ showOp True p o2 ++ " ? "
    ++ showOp True p eq ++ " : " ++ showOp True p ne ++ ";\n"
translate d p (ArithOp op r o1 o2) =
    showOp True p (Reg r) ++ " = (long int)" ++ showOp True p o1 ++ showArith op ++ "(long int)" ++ showOp True p o2 ++ ";\n"
translate d p x = "/* unknown `" ++ show x ++ "` in " ++ intercalate "." p ++ " */\n"

preheader :: String
preheader = unlines
    [ "#include \"gc.c\""
    , "int main() {"
    ]

postheader :: String
postheader = unlines
    [ "heap = alloc_arena(1024);"
    , "goto start;"
    ]

footer :: String
footer = "}\n"

moveStatic :: [Operator] -> ([Operator],[Operator])
moveStatic = partition (\case
    Table _ _ -> True
    _ -> False)

cgen :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
cgen cfg fs glue = do
    let (a,b) = unzip $ fmap (\(p,ops) -> let (static,ins) = moveStatic ops in ((p,static),(p,ins))) (([],glue):fmap (\(p,Right ops)->(p,ops)) fs)
    let statics = concatMap (\(p,s)->concatMap (translate False p) s) a
    let ins = concatMap (\(p,o)->concatMap (translate False p) o) b
    liftIO $ writeFile "c-build/main.c" (preheader ++ statics ++ postheader ++ ins ++ footer)
    liftIO $ putStrLn "main is: c-build/main.c"

cbackend :: Backend
cbackend = Backend cgen 100 (LocalIdentifier "start")