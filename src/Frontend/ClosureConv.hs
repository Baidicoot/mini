{-# LANGUAGE LambdaCase #-}
module Frontend.ClosureConv where

import Types.CPS
import Types.Ident

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map as Map

type Escaping = Set.Set Identifier
type Functions = Set.Set Identifier
-- cache of function -> fvs for known functions
-- iteration used to handle cycles
type Freevars = Map.Map Identifier (Set.Set Identifier)
-- argument capping is done in a separate pass
type ClosureConv = ReaderT (Escaping, Functions, FreeVars) (State Freevars)

escV :: Value -> Maybe Identifier
escV = (\case
    Var id -> Just id
    Label id -> Just id
    _ -> Nothing)

escaping :: CExp -> Escaping
escaping (App _ vs) = Set.fromList . catMaybes . fmap escV $ vs
escaping (Fix fns exp) = mappend (escaping exp) $ mconcat . fmap (\(Fun _ _ exp) -> escaping exp) $ fns
escaping (Record vs _ exp) = mappend (escaping exp) $ Set.fromList . catMaybes . fmap (escV . fst) $ vs
escaping (Select _ _ _ exp) = escaping exp
escaping (Switch _ exps) = mconcat (fmap escaping exps)
escaping (Primop _ vs _ exps) = mappend (escaping exp) $ Set.fromList . catMaybes . fmap escV $ vs
escaping _ = mempty

functions :: CExp -> Functions
functions (Fix fns exp) = mappend (Set.fromList . fmap fst $ fns) (functions exp)
functions (Record _ _ exp) = functions exp
functions (Select _ _ _ exp) = functions exp
functions (Switch _ exps) = mconcat . fmap functions $ exps
functions (Primop _ _ _ exps) = mconcat . fmap functions $ exps
functions _ = mempty

-- only handles CExp that it is expected to meet
fv :: CExp -> Set.Set Identifier
fv (App n vs) = Set.fromList . catMaybes . fmap escV $ (n:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . fmap (\(Fun id _ _) -> id) $ fns) $ Set.union (fv exp) $ fmap (\(Fun _ args exp) -> fv exp `Set.disjoint` (Set.fromList args))
fv (Record vs n exp) = Set.delete n $ (fv exp) `Set.union` (catMaybes . fmap (escV . fst) $ vs)
fv (Select _ v n exp) = Set.delete (LocalIdentifier n) $ Set.fromList (maybeToList (escV v)) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` (Set.fromList . maybeToList . escV $ v)
fv (Primop _ args n exps) = Set.delete (LocalIdentifier n) $ mconcat (fmap fv exps) `Set.union` (Set.fromList . catMaybes . fmap escV $ args)
fv _ = mempty

escapes :: Identifier -> ClosureConv Bool
escapes id = do
    (env, _) <- ask
    pure $ id `Set.member` env

isKnown :: Identifier -> ClosureConv Bool
isKnown id = do
    (_, env) <- ask
    pure $ id `Set.member` env

alwaysKnown :: Identifier -> ClosureConv Bool
alwaysKnown = fmap not . escapes