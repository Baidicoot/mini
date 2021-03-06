{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Backend.X86_64_linux.Textual (x86_64_linuxTextualBackend) where

import Backend.X86_64_linux.Types
import Backend.X86_64_linux.Translate
import Control.Applicative hiding(Const(..))
import Control.Monad.IO.Class
import Control.Monad

import Types.Abstract
import Types.Build
import Types.Ident

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

emitStr :: String -> X86 ()
emitStr s = X86 ((),s)

endl :: X86 ()
endl = emitStr "\n"

getReg :: Register -> String
getReg = ('%':) . regName

emitConst :: X86ConstOp -> X86 ()
emitConst (Label l) = emitStr (show l)
emitConst (Int i) = emitStr (show i)
emitConst (Char c) = emitStr (show c)
emitConst (Str s) = emitStr (show s)

emitOp :: X86Operand -> X86 ()
emitOp (Const c) = emitConst c
emitOp (Register (Indirect r)) = emitStr ("(" ++ getReg r ++ ")")
emitOp (Register (Direct r)) = emitStr (getReg r)
emitOp (Register (IndirectD r d)) = emitStr (show d ++ "(" ++ getReg r ++ ")")
emitOp (Register (IndirectDI r i d s)) = emitStr (show d ++ "(" ++ getReg r ++ "," ++ getReg i ++ "," ++ show s)

emitX86 :: X86Instruction -> X86 ()
emitX86 (DefLabel l) = emitStr (show l ++ ":") >> endl
emitX86 (PPC s) = emitStr ("# "++s) >> endl
emitX86 (Movq a b) = emitStr "movq " >> emitOp a >> emitStr ", " >> emitOp b >> endl
emitX86 (Long c) = emitStr ".long " >> emitConst c >> endl
emitX86 (DB c) = emitStr ".db " >> emitConst c >> endl
emitX86 (Jmpq o) = emitStr "jmp " >> emitOp o >> endl
emitX86 Syscall = emitStr "syscall" >> endl
emitX86 (Ccall s) = emitStr "call " >> emitStr s >> endl
emitX86 (Addq a b) = emitStr "addq " >> emitOp a >> emitStr ", " >> emitOp b >> endl
emitX86 (Subq a b) = emitStr "subq " >> emitOp a >> emitStr ", " >> emitOp b >> endl
emitX86 (Extern l) = emitStr (".extern " ++ show l) >> endl
emitX86 (Global l) = emitStr (".global " ++ show l) >> endl

abstToTexutalX86 :: [Operator] -> String
abstToTexutalX86 = (\(X86 (_,s))->s) . mapM emitX86 . concatMap translate

assembleX86 :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
assembleX86 cfg files glue = do
    paths <- forM files $ \case
        (p,Right ops) ->
            liftIO $ writeFile
                (root cfg ++ "cache-x86_64-linux-textual/" ++ concatMap (++".") p ++ "asm")
                (abstToTexutalX86 ops)
                >> pure (root cfg ++ "cache-x86_64-linux-textual/" ++ concatMap (++".") p ++ "asm")
    liftIO $ writeFile
        (root cfg ++ "cache-x86_64-linux-textual/" ++ "glue.asm")
        (abstToTexutalX86 glue)
    liftIO $ putStrLn ("main-is: " ++ root cfg ++ "cache-x86_64-linux-textual/" ++ "glue.asm")

x86_64_linuxTextualBackend :: Backend
x86_64_linuxTextualBackend = Backend assembleX86 nregs (LocalIdentifier (Symb "__start"))