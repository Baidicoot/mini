{-# LANGUAGE LambdaCase #-}
module Types.Pattern where

import Types.Ident
import Types.Graph

import qualified Data.Set as Set

data PatternNode
    = PatternCons Identifier
    | PatternVar Name
    | PatternWildcard
    deriving(Eq)

instance Show PatternNode where
    show (PatternCons id) = show id
    show (PatternVar n) = n
    show PatternWildcard = "_"

type SourcePattern = SourceGraph PatternNode
type Pattern = AppGraph PatternNode

vars :: Pattern -> Set.Set Name
vars = foldr Set.union Set.empty . fmap (\case
    PatternVar n -> Set.singleton n
    _ -> Set.empty)