module CPS.Meta (collect, reduce, FunctionMeta(..), FunctionClosure(..), allKnown, allEscaping) where

import Types.CPS
import Types.Ident
import Types.Prim

import Control.Arrow

import qualified Data.Set as Set
import qualified Data.Map as Map

data FunctionMeta = FunctionMeta
    { free :: Set.Set Name
    , escaping :: Set.Set Identifier
    , knownCalls :: Set.Set Name
    , unknownCalls :: Set.Set Name
    , nested :: Set.Set Name
    , bound :: Set.Set Name
    }
    deriving(Eq, Show)

emptyFunctionMeta :: FunctionMeta
emptyFunctionMeta = FunctionMeta mempty mempty mempty mempty mempty mempty

mergeFunctionMeta :: FunctionMeta -> FunctionMeta -> FunctionMeta
mergeFunctionMeta (FunctionMeta a c e g i k) (FunctionMeta b d f h j l) =
    FunctionMeta
        (a `Set.union` b)
        (c `Set.union` d)
        (e `Set.union` f)
        (g `Set.union` h)
        (i `Set.union` j)
        (k `Set.union` l)

binds :: [Name] -> FunctionMeta -> FunctionMeta
binds vars f = f
    { free = free f `Set.difference` (Set.fromList vars)
    , knownCalls = Set.filter (not . (`elem` vars)) (knownCalls f)
    , bound = bound f `Set.union` (Set.fromList vars)
    }

uses :: [Name] -> FunctionMeta -> FunctionMeta
uses vars f = f {free = free f `Set.union` (Set.fromList vars)}

escapes :: [Identifier] -> FunctionMeta -> FunctionMeta
escapes vars f = f {escaping = escaping f `Set.union` (Set.fromList vars)}

fixes :: [Name] -> FunctionMeta -> FunctionMeta
fixes fns f = f
    { free = free f `Set.difference` (Set.fromList fns)
    , unknownCalls = Set.filter (not . (`elem` fns)) (unknownCalls f)
    }

nests :: (Map.Map Identifier FunctionMeta) -> FunctionMeta -> FunctionMeta
nests m f = f
    { free = free f `Set.union` mconcat (fmap free fns)
    , knownCalls = knownCalls f `Set.union` mconcat (fmap knownCalls fns)
    , nested = nested f `Set.union` (Set.fromList names)
    }
    where
        fns = Map.elems m
        names = extractLocals $ Map.keys m

collectFunctionMeta :: CExp -> (Map.Map Identifier FunctionMeta, FunctionMeta)
collectFunctionMeta (Fix fns exp) =
    let (indirect, direct)
            = first mconcat
            . second Map.fromList
            . unzip
            $ map (\(Fun id args exp) ->
                (second ((,) id . binds args) (collectFunctionMeta exp))) fns
        (tailnests, this) = collectFunctionMeta exp
        fnNames = extractLocals $ map (\(Fun id _ _) -> id) fns
    in
        ( fmap (fixes fnNames) $ indirect `Map.union` direct `Map.union` tailnests
        , fixes fnNames . nests direct $ this
        )
collectFunctionMeta (App id@(Var (LocalIdentifier c)) vs) = (,) mempty $ emptyFunctionMeta
    { free = Set.fromList (extractNames (id:vs))
    , escaping = Set.fromList (extractIdents vs)
    , knownCalls = Set.singleton c
    , unknownCalls = Set.singleton c
    }
collectFunctionMeta (App id vs) = (,) mempty $ emptyFunctionMeta
    { free = Set.fromList (extractNames (id:vs))
    , escaping = Set.fromList (extractIdents vs)
    }
collectFunctionMeta (Select _ v n exp) =
    let (nestedfns, this) = collectFunctionMeta exp
    in (,) nestedfns . uses (extractNames [v]) . binds [n] $ this
collectFunctionMeta (Record paths n exp) =
    let (nestedfns, this) = collectFunctionMeta exp
        args = extractIdents $ fmap fst paths
    in (,) nestedfns . escapes args . uses (extractLocals args) . binds [n] $ this
collectFunctionMeta (Switch v exps) =
    let (nestedfns, this) =
            foldr (\(a,b) (c,d) -> (a `mappend` c, b `mergeFunctionMeta` d)) (mempty, emptyFunctionMeta) $
                fmap collectFunctionMeta exps
        args = extractNames [v]
    in (,) nestedfns . uses args $ this
collectFunctionMeta (Primop _ vs n exps) =
    let (nestedfns, this) =
            foldr (\(a,b) (c,d) -> (a `mappend` c, b `mergeFunctionMeta` d)) (mempty, emptyFunctionMeta) $
                fmap collectFunctionMeta exps
        args = extractIdents vs
    in (,) nestedfns . escapes args . uses (extractLocals args) . binds [n] $ this
collectFunctionMeta _ = (mempty, emptyFunctionMeta)

allKnown :: Map.Map Identifier FunctionMeta -> Set.Set Identifier
allKnown = Map.keysSet

allEscaping :: Map.Map Identifier FunctionMeta -> Set.Set Identifier
allEscaping = mconcat . fmap escaping . Map.elems

collect :: CExp -> Map.Map Identifier FunctionMeta
collect = fst . collectFunctionMeta

newtype FunctionClosure = FunctionClosure (Set.Set Name) deriving(Eq, Show)

-- fix this: - need to include fv from escaping, called that are not already bound by the function
-- this implies the need for a 'bound' field of the record
reduce :: Map.Map Identifier FunctionMeta -> Map.Map Identifier FunctionClosure
reduce
    = fmap (\(vars, _, _) -> FunctionClosure vars)
    . reduceMeta
    . fmap (\(FunctionMeta f e k _ _ b) ->
        (f, k `Set.union` (Set.fromList . extractLocals $ Set.toList e), b))
    where
        reduceOnce ::
            Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name)
            -> Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name)
        reduceOnce m
            = fmap (\(free, fns, bound) ->
                let (free', _, _)
                        = mconcat
                        . fmap (flip (Map.findWithDefault mempty) m . LocalIdentifier)
                        $ Set.toList fns
                in
                    (free `Set.union` (free' `Set.difference` bound), fns, bound)) m

        reduceMeta ::
            Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name)
            -> Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name)
        reduceMeta m
            | m' == m   = m'
            | otherwise = reduceMeta m'
            where
                m' = reduceOnce m