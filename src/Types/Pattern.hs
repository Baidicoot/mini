{-# LANGUAGE LambdaCase #-}
module Types.Pattern where

import Types.Ident
import Types.Graph
import Types.Prim

import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Parsec.Pos

data Pattern tag
    = PatternCons tag Identifier [Pattern tag]
    | PatternLit tag UnboxedLit
    | PatternVar tag Name
    | PatternWildcard tag
    deriving(Eq, Show)

type SourcePattern = Pattern SourcePos

prename :: Map.Map Name Name -> Pattern tag -> Pattern tag
prename m (PatternVar t n) = PatternVar t $ Map.findWithDefault n n m
prename m (PatternCons t i ps) = PatternCons t i $ fmap (prename m) ps
prename _ x = x

data PatternConstructor
    = ConsCons Identifier Int
    | ConsLit UnboxedLit
    | ConsWild
    | ConsVar Name
    deriving(Eq, Show)

pvars :: Pattern tag -> Set.Set Name
pvars (PatternCons _ _ ps) = mconcat (fmap pvars ps)
pvars (PatternVar _ n) = Set.singleton n
pvars _ = mempty

inc :: PatternConstructor -> PatternConstructor
inc (ConsCons id x) = ConsCons id (x+1)
inc x = x

cons :: Pattern tag -> PatternConstructor
cons (PatternCons _ id xs) = ConsCons id (length xs)
cons (PatternLit _ l) = ConsLit l
cons (PatternVar _ v) = ConsVar v
cons (PatternWildcard _) = ConsWild

fits :: PatternConstructor -> PatternConstructor -> Bool
_ `fits` ConsWild = True
x `fits` y = x == y

len :: PatternConstructor -> Int
len (ConsCons _ i) = i
len _ = 0

pargs :: Pattern tag -> [Pattern tag]
pargs (PatternCons _ _ xs) = xs
pargs _ = []