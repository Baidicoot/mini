{-# LANGUAGE MultiParamTypeClasses #-}
module Types.CPS where

import Types.Ident
import Types.Type
import Types.Prim
import Types.Pretty

import Data.List (intercalate)

data Value
    = Var Identifier
    | Label Identifier
    | Unboxed UnboxedLit
    deriving(Eq)

instance Show Value where
    show (Var id) = show id
    show (Label id) = show id
    show (Unboxed u) = show u

data CFun
    = Fun Identifier [Name] CExp
    deriving(Eq)

instance Show CFun where
    show (Fun n args exp) = show n ++ concatMap (\arg -> ' ':arg) args ++ " = " ++ show exp

data AccessPath
    = OffPath Int
    | SelPath Int AccessPath
    deriving(Eq)

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Name CExp
    | Select Int Value Name CExp
    -- | Offset Int Value Name CExp
    | Switch Value [CExp]
    | MatchError
    | Halt
    | Primop Primop [Value] Name [CExp]
    deriving(Eq)

instance Show CExp where
    show (App a args) = show a ++ concatMap (\arg -> ' ':show arg) args
    show (Fix defs exp) = "fix " ++ concatMap (\fn -> show fn ++ "\n") defs ++ "in " ++ show exp
    show (Record va id exp) = "let " ++ show id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show a ++ "=" ++ show v) va) ++ "} in " ++ show exp
    show (Select i v id exp) = "let " ++ show id ++ " = " ++ show v ++ "[" ++ show i ++ "]" ++ " in " ++ show exp
    show (Switch v exp) = "switch " ++ show v ++ concatMap (\cse -> "\n" ++ show cse) exp
    show Halt = "halt"
    show MatchError = "fail"

instance Show AccessPath where
    show (OffPath x) = show x
    show (SelPath x y) = show y ++ "[" ++ show x ++ "]"

instance Pretty CFun Int where
    pretty (Fun v args exp) n = intercalate " " (show v:args) ++ " = " ++ pretty exp (n+4)

instance Pretty CExp Int where
    pretty (App a args) _ = show a ++ concatMap (\arg -> ' ':show arg) args
    --"\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> v ++ " = " ++ pretty ir (n+4)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix defs exp) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\fn -> pretty fn (n+4)) defs) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Record va id exp) n = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show a ++ "=" ++ show v) va) ++ "}\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Select i v id exp) n = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ show v ++ "[" ++ show i ++ "]" ++ " in " ++ pretty exp (n+4)
    pretty (Switch v exp) n = "\n" ++ replicate n ' ' ++ "switch " ++ show v ++ concatMap (\(cse, i) -> "\n" ++ replicate (n+4) ' ' ++ show i ++ " -> " ++ pretty cse (n+8)) (zip exp [0..])
    pretty Halt _ = "halt"
    pretty MatchError _ = "fail"