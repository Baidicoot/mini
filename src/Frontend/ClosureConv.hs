{-# LANGUAGE LambdaCase #-}
module Frontend.ClosureConv where

import Types.CPS
import Types.Ident

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import qualified Data.Set as Set

type Escaping = Set.Set Identifier
type Env = Escaping
type ClosureState = [Name]
type ClosureConv = ReaderT Env (State ClosureState)

escV :: Value -> Maybe Identifier
escV = (\case
    Var id -> Just id
    Label id -> Just id
    _ -> Nothing)

escaping :: CExp -> Set.Set Identifier
escaping (App _ vs) = Set.fromList . catMaybes . fmap escV $ vs
escaping (Fix fns exp) = mappend (escaping exp) $ mconcat . fmap (\(Fun _ _ exp) -> escaping exp) $ fns
escaping (Record vs _ exp) = mappend (escaping exp) $ Set.fromList . catMaybes . fmap (escV . fst) $ vs
escaping (Select _ _ _ exp) = escaping exp
escaping (Switch _ exps) = mconcat (fmap escaping exps)
escaping (Primop _ vs _ exps) = mappend (escaping exp) $ Set.fromList . catMaybes . fmap escV $ vs
escaping _ = mempty

{-
fresh :: ClosureConv Name
fresh = do
    names <- get
    let (n:ns) = names
    put ns
    pure n

ftvId :: Identifier -> ClosureConv Env
ftvId id = do
    env <- ask
    pure $ if id `Set.member` env then Set.singleton id else mempty

ftvV :: Value -> ClosureConv Env
ftvV (Var id) = ftvId id
ftvV (Label id) = ftvId id
ftvV _ = pure mempty

ftvFn :: CFun -> ClosureConv Env
ftvFn (Fun _ args exp) = do
    let argenv = Set.fromList $ fmap LocalIdentifier args
    local (mappend argenv) (ftv exp)

ftv :: CExpr -> ClosureConv Env
ftv (App v vs) = do
    fvs <- mapM ftvV vs
    fv <- ftvV v
    pure $ foldr mappend fv fvs
ftv (Fix fns exp) = do
    let names = Set.fromList $ fmap (\(Fun n _ _) -> n) fns
    local (mappend names) $ do
        ffv <- mapM ftvFn fns
        fev <- ftv exp
        pure $ foldr mappend fev fvs
ftv (Record vs id cexp) = do
    fvs <- mapM (ftvV . fst) vs
    fev <- local (Set.insert id) (ftv cexp)
    pure $ foldr mappend fev fvs
-}