module CPS.Meta (collect, reduce, ClosureData) where

import Types.CPS
import Types.Ident

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Maybe
import Data.Foldable

import qualified Data.Set as Set
import qualified Data.Map as Map

{-
'Types' of variable:
- free variables - variables that are not bound in the function
- (all) escaping variables - variables that are arguments to operators
- known calls
- unknown calls
- calls that might capture
-}

type Collector = ReaderT CollectEnv (Writer (Set.Set Name))

type Metadata = Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)
-- fv, escaping, known calls, unknown calls, calls that might capture

type CollectEnv =
    (Maybe Identifier, Set.Set Name)
-- current function name, known functions

knownF :: Name -> Collector Bool
knownF n = do
    (_, env) <- ask
    pure $ n `Set.member` env

update :: ((Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name) -> (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)) -> Metadata -> Collector Metadata
update f m = do
    env <- ask
    case env of
        (Nothing, _) -> pure m
        (Just n, _) -> pure $ Map.update (\(a, b, c, d, e) -> Just $ f (a, b, c, d, e)) n m

includes :: Set.Set Name -> Metadata -> Collector Metadata
includes ns m = do
    ns' <- filterM (fmap not . knownF) (Set.toList ns)
    ns'' <- filterM knownF (Set.toList ns)
    update (\(a, b, c, d, e) -> (a `Set.union` (Set.fromList ns'), b, c, d, e `Set.union` (Set.fromList ns''))) m

binds :: Name -> Metadata -> Collector Metadata
binds n = update (\(a, b, c, d, e) -> (n `Set.delete` a, b, c, d, e))

fnArgs :: [Name] -> Metadata -> Collector Metadata
fnArgs ns = update (\(a, b, c, d, e) -> (a `Set.difference` (Set.fromList ns), b, c, d, e))

root :: Collector Metadata
root = do
    env <- ask
    case env of
        (Nothing, _) -> pure mempty
        (Just n, _) -> pure (Map.singleton n mempty)

calls :: Set.Set Name -> Metadata -> Collector Metadata
calls n m = do
    fs <- foldM (\b n -> fmap (. b) $ do
        isKnown <- knownF n
        pure $ if isKnown then
                (\(a, b, c, d, e) -> (a, b, Set.insert n c, d, e))
            else
                (\(a, b, c, d, e) -> (a, b, c, Set.insert n d, e))) id n
    includes n =<< update fs m

hasArgs :: Set.Set Name -> Metadata -> Collector Metadata
hasArgs ns = update (\(a, b, c, d, e) -> (a, b `Set.union` ns, c, d, e))

inFn :: Identifier -> Collector a -> Collector a
inFn a = local (\(_, b) -> (Just a, b))

withKnown :: Set.Set Name -> Collector a -> Collector a
withKnown ns = local (\(a, b) -> (a, b `Set.union` ns))

collectM :: CExp -> Collector Metadata
collectM (Fix fns exp) = do
    let names = Set.fromList . catMaybes $ fmap (\(Fun id _ _) -> identToName id) fns
    tell names
    m <- withKnown names $ collectM exp
    withKnown names $ foldM (\m (Fun id args exp) -> fmap (mappend m) . inFn id $ fnArgs args =<< collectM exp) m fns
collectM (App v vs) = do
    let vs' = valuesToSet vs
    let v' = valueToSet v
    calls v' =<< hasArgs vs' =<< root
collectM (Record paths n exp) = do
    let vs = valuesToSet $ fmap fst paths
    hasArgs vs =<< binds n =<< collectM exp
collectM (Select _ v n exp) = do
    let v' = valueToSet v
    hasArgs v' =<< binds n =<< collectM exp
collectM (Switch v exps) = do
    let v' = valueToSet v
    hasArgs v' =<< foldM (\b -> fmap (mappend b) . collectM) mempty exps
collectM (Primop _ vs n exps) = do
    let vs' = valuesToSet vs
    hasArgs vs' =<< binds n =<< foldM (\b -> fmap (mappend b) . collectM) mempty exps
collectM _ = root

collect :: CExp -> (Metadata, Set.Set Name)
collect e = runWriter $ runReaderT (collectM e) (Nothing, mempty)

type ClosureData = Map.Map Identifier (Set.Set Name, Set.Set Name, Set.Set Name, Set.Set Name)
-- fv, escaping, known calls, unknown calls

reduce :: Metadata -> ClosureData
reduce = fmap (\(a, b, c, d, _) -> (a, b, c, d)) . reduceFully
    where
        reduceFully m = let m' = reduceOnce m in if m == m' then m else reduceFully m'
        reduceOnce m = fmap (\(fv, b, c, d, needs) ->
            let (fv', needs') = foldr mappend (fv, needs) . fmap (\need -> case Map.lookup (LocalIdentifier need) m of
                    Just (fv, _, _, _, needs) -> (fv, needs)
                    Nothing -> mempty) $ Set.toList needs
            in (fv', b, c, d, needs')) m