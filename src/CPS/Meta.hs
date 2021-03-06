module CPS.Meta where

import Types.CPS
import Types.Ident
import Types.Prim

import Control.Arrow

import qualified Data.Set as Set
import qualified Data.Map as Map

getCaptured :: CExp -> Map.Map Identifier FV
getCaptured = fmap fst . reduce . fst . collectPerFunctionMeta

getBound :: CExp -> Map.Map Identifier FV
getBound = fmap snd . reduce . fst . collectPerFunctionMeta

data PerFunction = PerFunctionData
    { free :: Set.Set Identifier
    , bound :: Set.Set Identifier -- local variables are not top-level
    , knownCalls :: Set.Set Identifier -- known vs unknown calls determined at CPSification
    , unknownCalls :: Set.Set Identifier
    , knownEsc :: Set.Set Identifier -- known escaping functions
    , nested :: Set.Set Identifier
    }
    deriving(Show)

instance Semigroup PerFunction where
    (PerFunctionData f b k u e n) <> (PerFunctionData f' b' k' u' e' n') = PerFunctionData
        (f <> f')
        (b <> b')
        (k <> k')
        (u <> u')
        (e <> e')
        (n <> n')

instance Monoid PerFunction where
    mempty = PerFunctionData mempty mempty mempty mempty mempty mempty
    mappend = (<>)

uses :: [Identifier] -> PerFunction -> PerFunction
uses ids = mappend (mempty {free = Set.fromList ids})

passes :: [Identifier] -> PerFunction -> PerFunction
passes ids = mappend (mempty {knownEsc = Set.fromList ids})

binds :: [Identifier] -> PerFunction -> PerFunction
binds ids f = f
    { bound = bound f `mappend` Set.fromList ids }

nests :: Map.Map Identifier PerFunction -> PerFunction -> PerFunction
nests m = mappend mempty {bound = mconcat $ fmap bound fns, free = mconcat $ fmap free fns, knownCalls = mconcat $ fmap knownCalls fns, knownEsc = mconcat $ fmap knownEsc fns, nested = Set.fromList $ Map.keys m}
    where
        fns = Map.elems m

calls :: Value -> PerFunction -> PerFunction
calls (Label i) = mappend mempty {knownCalls = Set.singleton i}
calls (Var i) = mappend mempty {unknownCalls = Set.singleton i}
calls _ = mappend mempty

collectFunctionMeta :: CFun -> (Map.Map Identifier PerFunction, (Identifier,PerFunction))
collectFunctionMeta (Fun _ i args e) =
    let (nested, self) = collectPerFunctionMeta e
    in (nested, (i,binds args self))

collectPerFunctionMeta :: CExp -> (Map.Map Identifier PerFunction, PerFunction)
collectPerFunctionMeta (Fix defs e) =
    let (metanests, nested) = unzip $ fmap collectFunctionMeta defs
        (trailing, self) = collectPerFunctionMeta e
        nested' = Map.fromList nested
    in (mconcat metanests <> trailing <> nested', nests nested' self)
collectPerFunctionMeta (Record vs n e) =
    second (passes (extractLabels $ fmap fst vs) . uses (extractIdents $ fmap fst vs) . binds [n]) (collectPerFunctionMeta e)
collectPerFunctionMeta (Select i v n e) =
    second (uses (extractIdents [v]) . binds [n]) (collectPerFunctionMeta e)
collectPerFunctionMeta (Switch v es) =
    second (uses (extractIdents [v])) (mconcat $ fmap collectPerFunctionMeta es)
collectPerFunctionMeta (Primop op vs n es) =
    second (uses (extractIdents vs) . binds [n]) (mconcat $ fmap collectPerFunctionMeta es)
collectPerFunctionMeta (App v vs) = (mempty, calls v . passes (extractLabels vs) $ uses (extractIdents (v:vs)) mempty)
collectPerFunctionMeta (Error s) = mempty
collectPerFunctionMeta Halt = mempty

type FV = Set.Set Identifier

reduce :: Map.Map Identifier PerFunction -> Map.Map Identifier (FV,FV)
reduce
    = fmap (\(_,b,f) -> (f `Set.difference` b,b))
    . internal
    . fmap (\(PerFunctionData f b k _ e _) -> (k `Set.union` e, b, f `Set.difference` b))
    where
        internalStep ::
            Map.Map Identifier (Set.Set Identifier, Set.Set Identifier, Set.Set Identifier)
            -> Map.Map Identifier (Set.Set Identifier, Set.Set Identifier, Set.Set Identifier)
        internalStep m = fmap (\(c,b,f) ->
            let (_,_,f') = mconcat $ fmap (\c -> Map.findWithDefault mempty c m) (Set.toList c)
            in (c,b,f `Set.union` (f' `Set.difference` b))) m
        
        internal ::
            Map.Map Identifier (Set.Set Identifier, Set.Set Identifier, Set.Set Identifier)
            -> Map.Map Identifier (Set.Set Identifier, Set.Set Identifier, Set.Set Identifier)
        internal m
            | m' == m = m
            | otherwise = internal m'
            where
                m' = internalStep m