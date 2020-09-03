{-# LANGUAGE MultiParamTypeClasses #-}
module Types.CPS where

import Types.Ident
import Types.Type
import Types.Literal
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
    deriving(Eq, Show)

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Identifier CExp
    | Select Int Value Identifier CExp
    | Offset Int Value Identifier CExp
    | Switch Value [CExp]
    -- | Primop Primop [Value] [Identifier] [CExp]
    deriving(Eq)

instance Show CExp where
    show (App a args) = show a ++ concatMap (\arg -> ' ':show arg) args
    show (Fix defs exp) = "fix " ++ concatMap (\fn -> show fn ++ "\n") defs ++ "in " ++ show exp

instance Pretty CFun Int where
    pretty (Fun v args exp) n = intercalate " " (show v:args) ++ " = " ++ pretty exp (n+4)

instance Pretty CExp Int where
    pretty (App a args) _ = show a ++ concatMap (\arg -> ' ':show arg) args
    --"\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> v ++ " = " ++ pretty ir (n+4)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix defs exp) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\fn -> pretty fn (n+4)) defs) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)