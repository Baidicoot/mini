{-# LANGUAGE LambdaCase #-}

module Backend.C where

import Types.Abstract
import Types.CPS (AccessPath(..))
import Types.Ident
import qualified Types.Prim as Prim
import Data.List

import Control.Monad.IO.Class

import Types.Build

import Control.Arrow

mangle :: ModulePath -> Label -> String
mangle p (LocalIdentifier l) = concatMap (++"_") p ++ l
mangle p (ExternalIdentifier m l) = concatMap (++"_") m ++ l

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

translateOp :: Bool -> ModulePath -> Operand -> Offset -> String
translateOp b mp op (POff p) = showPath p
    where
        showPath (SelPath i x) = "((void**)(" ++ showPath x ++ "))[" ++ show i ++ "]"
        showPath NoPath = showOp b mp op
translateOp b mp op1 (OOff op2) = "((void**)(" ++ showOp b mp op1 ++ "))[(long int)" ++ showOp b mp op2 ++ "]"

translate :: Bool -> ModulePath -> Operator -> String
translate d p (Table t xs) = "void* " ++ mangle p t ++ "[] = {"  ++ intercalate "," (fmap (\case
    EmitLit l -> showLit l
    EmitLabel l 0 -> "&&" ++ mangle p l
    EmitLabel l o -> "&&" ++ mangle p l ++ " + " ++ show o) xs) ++ "};\n"
translate d p (Define l) = mangle p l ++ ": ;\n" ++ if d then "printf(\"entered " ++ show l ++ "\\n\");\n" else ""
translate d p (Comment s) = if d then  "/* " ++ s ++ " */\n" else ""
translate d p (Jmp (ImmLabel l)) = "goto " ++ mangle p l ++ ";\n"
translate d p (Jmp o) = "reg_arith = " ++ showOp True p o ++ ";\ngoto *reg_arith" ++ ";\n"
translate d p (Record ps r) = showOp True p (Reg r) ++ " = malloc(sizeof(void*)*" ++ show (length ps) ++ ");\n"
    ++ concatMap (\((o,pa),i)->translateOp True p (Reg r) (POff $ SelPath i NoPath) ++ " = " ++ translateOp True p o (POff pa) ++ ";\n") (zip ps [0..])
    ++ if d then "printf(\"allocated " ++ show (length ps) ++ " in " ++ show r ++ "\\n\");\n" else ""
translate d p (Select i o r) = showOp True p (Reg r) ++ " = " ++ translateOp True p o (POff $ SelPath i NoPath) ++ ";\n"
translate d p (Fetch r o1 o2) =
    showOp True p (Reg r) ++ " = " ++ translateOp False p o1 (OOff o2) ++ ";\n"
    ++ if d then "printf(\"%x\\n\", " ++ showOp True p (Reg r) ++ ");\n" else ""
translate d p Halt = "return 0;\n"
translate d p (Error s) = "printf(" ++ show (s++"\n") ++ ");return 1;\n"
translate d p (Move r op) = showOp True p (Reg r) ++ " = " ++ showOp True p op ++ ";\n"
translate d _ (Exports _) = ""
translate d _ (Imports _) = ""
translate d p x = if d then "/* unknown `" ++ show x ++ "` in " ++ intercalate "." p ++ " */\n" else ""

preheader :: String
preheader = "void* reg_gpr[100];\nvoid* reg_arith;\nvoid* data_ptr;\nvoid* data_lim;\nint main() {\n"

footer :: String
footer = "}\n"

moveStatic :: [Operator] -> ([Operator],[Operator])
moveStatic = partition (\case
    Table _ _ -> True
    _ -> False)

cgen :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build String
cgen cfg fs glue = do
    let (a,b) = unzip $ fmap (\(p,ops) -> let (static,ins) = moveStatic ops in ((p,static),(p,ins))) (([],glue):fmap (\(p,Right ops)->(p,ops)) fs)
    let statics = concatMap (\(p,s)->concatMap (translate False p) s) a
    let ins = concatMap (\(p,o)->concatMap (translate False p) o) b
    liftIO $ writeFile (root cfg ++ "main.c") (preheader ++ statics ++ "goto start;\n" ++ ins ++ footer)
    pure (root cfg ++ "main.c")

cbackend :: Backend
cbackend = Backend cgen 100 (LocalIdentifier "start")