module CPS.Meta where

import Types.CPS
import Types.Ident
import Types.Prim

import Control.Arrow

import qualified Data.Set as Set
import qualified Data.Map as Map

data PerFunction = PerFunctionData
    { free :: Set.Set Identifier
    , bound :: Set.Set Name -- local variables are not top-level
    , knownCalls :: Set.Set Identifier -- known vs unknown calls determined at CPSification
    , unknownCalls :: Set.Set Identifier
    , escapes :: Set.Set Identifier
    , nested :: Set.Set Name -- nested functions are not top-level (and therefore are mangled)
    }

instance Semigroup PerFunction where
    (PerFunctionData f b c e n) <> (PerFunctionData f' b' c' e' n') = PerFunctionData
        (f <> f')
        (b <> b')
        (c <> c')
        (e <> e')
        (n <> n')

instance Monoid PerFunction where
    mempty = PerFunctionData mempty mempty mempty mempty mempty
    mappend = (<>)

uses :: [Identifier] -> PerFunction -> PerFunction
uses ids = mappend mempty {free = Set.fromList ids}

passes :: [Identifier] -> PerFunction -> PerFunction
passes ids = mappend mempty {escapes = Set.fromList ids}

binds :: [Name] -> PerFunction -> PerFunction
binds ids f = f
    { free = free f `Set.difference` Set.fromList ids
    , bound = Set.fromList ids }

callsKnown :: CallValue -> PerFunction -> PerFunction
callsKnown (Known i) = mappend mempty {knownCalls = Set.singleton i, free = Set.singleton i}
callsKnown (Unknown i) = mappend mempty {unknownCalls = Set.singleton i}

nests :: Map.Map Identifier PerFunction -> PerFunction
nests m = mappend mempty {free = concatMap free fns, knownCalls = concatMap knownCalls fns, nested = Map.keys m}
    where
        fns = Map.elems m

collectFunctionMeta :: CFun -> PerFunction
collectFunctionMeta (Fun i args e) =
    let (nested, self) = collectPerFunctionMeta e in 

collectPerFunctionMeta :: CExp -> (Map.Map Identifier PerFunction, PerFunction)
collectPerFunctionMeta (Fix defs e) =
    let nested = map (\(Fun i args e) -> let (collectPerFunctionMeta e)