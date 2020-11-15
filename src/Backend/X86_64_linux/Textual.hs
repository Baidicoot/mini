{-# LANGUAGE FlexibleInstances #-}

module Backend.X86_64_linux.Textual where

import Backend.Backend
import Control.Monad
import Control.Applicative

import Types.Prim
import Types.Abstract
import Types.CPS (AccessPath(..))

newtype X86_64_linux a = X86 (a, String)

type X86 = X86_64_linux

instance Functor X86 where
    fmap f (X86 (a,s)) = X86 (f a,s)

instance Applicative X86 where
    pure a = X86 (a,mempty)
    liftA2 f (X86 (a,as)) (X86 (b,bs)) = X86 (f a b,as ++ bs)

instance Monad X86 where
    return = pure
    (X86 (a,s)) >>= f = let (X86 (b,s')) = f a in X86 (b,s++s')

tell :: String -> X86 ()
tell s = X86 ((),s)

ins :: String -> X86 ()
ins = tell . ("\n    "++)

op :: Operand -> String
op (ImmLabel l) = show l
op (ImmLit (Int i)) = show i
op (ImmLit Unit) = "0"
op (Reg r) = '%':reg r

reg :: Register -> String
reg DataLim = "r15"
reg DataPtr = "r14"
reg Arith = "r13"
reg (GPR i) = 'r':show i

access :: Operand -> AccessPath -> String
access o (OffPath 0) = op o
access o (OffPath i) = show (i*4) ++ "(" ++ op o ++ ")"
access o (SelPath i p) = show (i*4) ++ "(" ++ access o p ++ ")"

saveRegsForAlloc :: X86 ()
saveRegsForAlloc = tell "\nsave for alloc"

restoreRegsForAlloc :: X86 ()
restoreRegsForAlloc = tell "\nrestore for alloc"

alloc :: Int -> X86 ()
alloc i = tell ("\nallocate " ++ show (i*4))

syscall :: [String] -> X86 ()
syscall xs = tell ("\nsyscall " ++ concat xs)

convertOp :: Operator -> X86 ()
convertOp (EmitLit Unit) = ins ".long 0"
convertOp (EmitLit (Int i)) = ins (".long " ++ show i)
convertOp (EmitPtr l i) = ins (".long " ++ show l ++ " + " ++ show i)
convertOp (Define l) = tell ('\n':show l ++ ":")
convertOp (Comment s) = tell ('\n':'#':s)
convertOp (Jmp o) = ins ("jmp " ++ op o)
convertOp (Record os r) = do
    saveRegsForAlloc
    alloc (length os)
    restoreRegsForAlloc
    forM_ (zip os [0..]) $ \((o,p),i) -> do
        ins ("movq " ++ access o p ++ "," ++ show i ++ "(%rax)")
    ins ("movq %rax,%" ++ reg r)
convertOp (Select i o r) = ins ("movq " ++ show i ++ "(" ++ op o ++ "),%" ++ reg r)
convertOp (Fetch r o1 o2) = ins ("movq (" ++ op o1 ++ "," ++ op o2 ++ ",4),%" ++ reg r)
convertOp (Move r o) = ins ("movq " ++ op o ++ ",%" ++ reg r)
convertOp Halt = syscall ["60"]
convertOp (Error s) = syscall ["60",s]
convertOp _ = pure ()

instance Backend (X86 ()) where
    codegen = mapM_ convertOp
    emit (X86 (_,s)) = s