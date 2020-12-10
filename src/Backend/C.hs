{-# LANGUAGE LambdaCase #-}

module Backend.C where

import Types.Abstract
import Types.CPS (AccessPath(..))
import Types.Ident
import qualified Types.Prim as Prim
import Data.List

import Control.Monad.IO.Class

import Types.Build

mangle :: ModulePath -> Label -> String
mangle p (LocalIdentifier l) = concatMap (++"_") p ++ l
mangle p (ExternalIdentifier m l) = concatMap (++"_") m ++ l

data Offset
    = POff AccessPath
    | OOff Operand

showLit :: Prim.UnboxedLit -> String
showLit (Prim.Char c) = '\'':c:'\'':""
showLit (Prim.Int i) = show i

showOp :: ModulePath -> Operand -> String
showOp p (Reg Arith) = "reg_arith"
showOp p (Reg DataPtr) = "data_ptr"
showOp p (Reg DataLim) = "data_lim"
showOp p (Reg (GPR n)) = "reg_gpr[" ++ show n ++ "]"
showOp p (ImmLabel l) = "&&" ++ mangle p l
showOp p(ImmLit l) = showLit l

translateOp :: ModulePath -> Operand -> Offset -> String
translateOp mp op (POff p) = showPath p
    where
        showPath (SelPath i x) = "((void**)(" ++ showPath x ++ "))[" ++ show i ++ "]"
        showPath NoPath = showOp mp op
translateOp mp op1 (OOff op2) = "((void**)(" ++ showOp mp op1 ++ "))[" ++ showOp mp op2 ++ "]"

translate :: Bool -> ModulePath -> Operator -> String
translate d p (Table t xs) = "void* " ++ mangle p t ++ intercalate "," (fmap (\case
    EmitLit l -> showLit l
    EmitLabel l 0 -> mangle p l
    EmitLabel l o -> mangle p l ++ " + " ++ show o) xs) ++ "\n"
translate d p (Define l) = mangle p l ++ ": ;\n" ++ if d then "printf(\"entered " ++ show l ++ "\\n\");\n" else ""
translate d p (Comment s) = if d then  "/* " ++ s ++ " */\n" else ""
translate d p (Jmp (ImmLabel l)) = "goto " ++ mangle p l ++ ";\n"
translate d p (Jmp o) = "reg_arith = " ++ showOp p o ++ ";\ngoto *reg_arith" ++ ";\n"
translate d p (Record ps r) = showOp p (Reg r) ++ " = malloc(sizeof(void*)*" ++ show (length ps) ++ ");\n"
    ++ concatMap (\((o,pa),i)->translateOp p (Reg r) (POff $ SelPath i NoPath) ++ " = " ++ translateOp p o (POff pa) ++ ";\n") (zip ps [0..])
    ++ if d then "printf(\"allocated " ++ show (length ps) ++ " in " ++ show r ++ "\\n\");\n" else ""
translate d p (Select i o r) = showOp p (Reg r) ++ " = " ++ translateOp p o (POff $ SelPath i NoPath) ++ ";\n"
translate d p (Fetch r o1 o2) = showOp p (Reg r) ++ " = " ++ translateOp p o1 (OOff o2) ++ ";\n"
translate d p Halt = "return 0;\n"
translate d p (Error s) = "printf(\"" ++ s ++ "\");return 1;\n"
translate d p (Move r op) = showOp p (Reg r) ++ " = " ++ showOp p op ++ ";\n"
translate d _ (Exports _) = ""
translate d _ (Imports _) = ""
translate d p x = if d then "/* unknown `" ++ show x ++ "` in " ++ intercalate "." p ++ " */\n" else ""

header :: String
header = "void* reg_gpr[100];\nvoid* reg_arith;\nvoid* data_ptr;\nvoid* data_lim;\nint main() {\ngoto start;\n"

footer :: String
footer = "}\n"

cgen :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build String
cgen cfg fs glue = do
    let p = concatMap (\(p,Right ops) -> concatMap (translate False p) ops) fs
    let b = concatMap (translate False []) glue ++ p
    liftIO $ writeFile (root cfg ++ "main.c") (header ++ b ++ footer)
    pure (root cfg ++ "main.c")

cbackend :: Backend
cbackend = Backend cgen 100 (LocalIdentifier "start")