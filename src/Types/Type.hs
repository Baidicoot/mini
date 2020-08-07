{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Type where

import Types.Ident
import Types.Pattern
import Types.Graph

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

data TypeNode
    = FunctionType
    | NamedType Identifier
    | TypeVar Name
    deriving(Eq)

type Type = AppGraph TypeNode

type Kind = Type

instance Show TypeNode where
    show FunctionType = "->"
    show (NamedType s) = show s
    show (TypeVar s) = s

instance {-# OVERLAPPING #-} Show Type where
    show (App () (Node () FunctionType) (Node () x)) = show x ++ " ->"
    show (App () (Node () FunctionType) x) = "(" ++ show x ++ ") ->"
    show (App () a (Node () b)) = show a ++ " " ++ show b
    show (App () a b) = show a ++ " (" ++ show b ++ ")"
    show (Node () x) = show x