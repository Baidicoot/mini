module Types.Type where

import Types.Ident

data Type
    = TypeApp Type Type
    | FunctionType
    | NamedType Identifier
    | TypeVar Name
    | SeqType [Type]
    deriving(Eq)

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