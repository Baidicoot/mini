module Backend.X86_64_linux.Types where

import Types.Abstract

rax :: Register
rax = GPR 0

arith :: Register
arith = GPR 1

data AddrMode
    = IndirectDI Register Register Int Int
    | IndirectD Register Int
    | Indirect Register
    | Direct Register

data X86ConstOp
    = Label Label Int
    | Int Int
    | Char Char
    | Str String

data X86Operand
    = Const X86ConstOp
    | Register AddrMode

data X86Instruction
    = DefLabel Label
    | PPC String
    | Movq X86Operand X86Operand
    | Long X86ConstOp
    | DB X86ConstOp
    | Jmpq X86Operand
    | SaveRegs [Register]
    | RestoreRegs [Register]
    | Syscall
    | UntrashStack
    | Ccall String