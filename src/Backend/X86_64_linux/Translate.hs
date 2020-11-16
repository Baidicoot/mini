module Backend.X86_64_linux.Translate where

import Backend.X86_64_linux.Types

import Types.Abstract
import Types.CPS (AccessPath(..))
import qualified Types.Prim as Prim

-- primops, jumps and switches take the 'direct' (actual values) of their operands
translateOpDirect :: Operand -> X86Operand
translateOpDirect (Reg r) = Register (Direct r)
translateOpDirect (ImmLabel l) = Const (Label l 0)
translateOpDirect (ImmLit (Prim.Int i)) = Const (Int i)
translateOpDirect (ImmLit (Prim.Char c)) = Const (Char c)
translateOpDirect (ImmLit Prim.Unit) = Const (Int 0)

-- most other things take the 'indirect' (pointed-to value) of their operands
translateOp :: Operand -> X86Operand
translateOp (Reg r) = Register (Indirect r)
translateOp (ImmLabel l) = Const (Label l 0)
translateOp (ImmLit (Prim.Int i)) = Const (Int i)
translateOp (ImmLit (Prim.Char c)) = Const (Char c)
translateOp (ImmLit Prim.Unit) = Const (Int 0)

translateAccess :: Operand -> AccessPath -> X86Operand -> [X86Instruction]
translateAccess o NoPath dst = [ Movq (translateOp o) dst ]
translateAccess (Reg r) (SelPath d NoPath) dst = [ Movq (Register (IndirectD r (d*4))) dst ]
translateAccess o (SelPath d NoPath) dst =
    [ Movq (translateOp o) (Register (Direct arith))
    , Movq (Register (IndirectD arith (d*4))) dst ]
translateAccess o (SelPath d p) dst =
    translateAccess o p (Register (Direct arith))
    ++ [ Movq (Register (IndirectD arith (d*4))) dst ]

saveExcept :: [Register] -> [X86Instruction]
saveExcept rs =
    Subq (Register (Direct rsp)) (Const (Int callerFrame))
    : [ Movq (Register (Direct r)) (Register (IndirectD rsp (i*4))) | (r,i) <- zip (filter (`notElem` rs) callerSaved) [0..] ]

restoreExcept :: [Register] -> [X86Instruction]
restoreExcept rs =
    [ Movq (Register (IndirectD rsp (i*4))) (Register (Direct r)) | (r,i) <- zip (filter (`notElem` rs) callerSaved) [0..] ]
    ++ [ Addq (Register (Direct rsp)) (Const (Int callerFrame)) ]

translate :: Operator -> [X86Instruction]
translate (EmitLit (Prim.Int i)) = [Long (Int i)]
translate (EmitLit (Prim.Char c)) = [DB (Char c)]
translate (EmitLit Prim.Unit) = [Long (Int 0)]
translate (EmitPtr l i) = [Long (Label l i)]
translate (Define l) = [DefLabel l]
translate (Comment s) = [PPC s]
translate (Jmp o) = [Jmpq (translateOpDirect o)]
translate (Record ps r) =
    saveExcept [r]
    ++ [ Ccall "alloc", Movq (Register (Direct rax)) (Register (Direct r)) ]
    ++ restoreExcept [r]
    ++ concatMap (\((o,p),i) -> translateAccess o p (Register (IndirectD r (i*4)))) (zip ps [0..])
translate (Select i o r) = translateAccess o (SelPath i NoPath) (Register (Direct r))
translate (Fetch r o1 o2) = []
translate Halt = [ Movq (Const (Int 60)) (Register (Direct rax)), Syscall ]
translate (Error s) = [ Movq (Const (Int 60)) (Register (Direct rax)), Syscall ]
translate _ = []