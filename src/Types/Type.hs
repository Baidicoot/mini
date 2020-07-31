module Types.Type where

import Types.Ident
import Types.Pattern

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

data Type
    = TypeApp Type Type
    | FunctionType
    | LazyType
    | NamedType Identifier
    | TypeVar Name
    | SeqType [Type]
    deriving(Eq)

type Constraint = (Type, Type)

data TypeError
  = UnificationFail Type Type
  | InfiniteType Name Type
  | UnboundIdent Identifier
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  | PatternUnderflow Pattern [Pattern]
  deriving(Show)

pshow_type :: Type -> String
pshow_type f@(TypeApp _ _) = "(" ++ show f ++ ")"
pshow_type x = show x

instance Show Type where
    show (TypeApp (TypeApp FunctionType a) b) = pshow_type a ++ " -> " ++ pshow_type b
    show (TypeApp a b) = pshow_type a ++ " " ++ show b
    show (NamedType s) = show s
    show (TypeVar s) = s
    show (SeqType xs) = show xs
    show FunctionType = "(->)"