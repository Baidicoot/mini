module Backend.X86_64_linux.Types where

import Types.Abstract

{-
Register Mappings:
Abstract    X86     Callee-Saved
00          rax     => used as return register
01          rcx     no
02          rbx     yes
03          rdx     no
04          rsi     no
05          rdi     no
06          rbp     yes
07          r8      no
08          r9      no
09          r10     no
10          r11     no
11          r12     yes
12          r13     yes
13          r14     yes
14          rsp     no
arith       r15     => not required to be saved
-}

callerSaved :: [Register]
callerSaved = GPR <$> [0,1,3,4,5,7,8,9,10,14]

callerFrame :: Int
callerFrame = 10*4

regName :: Register -> String
regName (GPR 0) = "rax"
regName (GPR 1) = "rcx"
regName (GPR 2) = "rbx"
regName (GPR 3) = "rdx"
regName (GPR 4) = "rsi"
regName (GPR 5) = "rdi"
regName (GPR 6) = "rbp"
regName (GPR 14) = "rsp"
regName Arith = "r15"
regName (GPR i) = 'r':show (i-1)

rax :: Register
rax = GPR 0

rsp :: Register
rsp = GPR 14

arith :: Register
arith = GPR 15

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
    | Global Label
    | Extern Label
    | PPC String
    | Movq X86Operand X86Operand
    | Long X86ConstOp
    | DB X86ConstOp
    | Jmpq X86Operand
    | Addq X86Operand X86Operand
    | Subq X86Operand X86Operand
    | Syscall
    | Ccall String