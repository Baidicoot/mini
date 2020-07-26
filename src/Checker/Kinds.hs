module Checker.Kinds where

import Types.Type
import Types.Kind
import Types.Ident

import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set

funcKind :: Kind
funcKind = KindFunc KindAny (KindFunc KindAny KindStar)

lazyKind :: Kind
lazyKind = KindFunc KindAny KindStar

desugarseq :: Map.Map Identifier Kind -> Type -> Either [Identifier] TypeUnifier
desugarseq env (TypeApp (TypeApp FunctionType (SeqType [])) x) = do
    x' <- desugarseq env x
    pure (UApp (UKind lazyKind) x')
desugarseq env (TypeApp (TypeApp FunctionType (SeqType xs@(_:_))) x) = do
    x' <- desugarseq env x
    xs' <- mapM (desugarseq env) xs
    foldM (\a x -> pure (UApp (UApp (UKind funcKind) x) (USeq [a]))) (UApp (UApp (UKind funcKind) (last xs')) x') (reverse (init xs'))
desugarseq env (TypeApp a b) = do
    a' <- desugarseq env a
    b' <- desugarseq env b
    pure (UApp a' b')
desugarseq env (SeqType xs) = do
    xs' <- mapM (desugarseq env) xs
    pure (USeq xs')
desugarseq env (NamedType s) = case Map.lookup s env of
    Just x -> Right (UKind x)
    Nothing -> Left [s]
desugarseq env (TypeVar s) = pure (UVar s)
desugarseq env FunctionType = pure (UKind funcKind)

-- Kind Inference Monad
data KindError
    = UnificationError Kind Kind String
    | RebindError Kind Kind Name
    | NotFunction Kind String
    | NotInScope Name
    | InferError (Set.Set String)
    deriving(Eq, Show)

data TypeUnifier
    = UKind Kind
    | UApp TypeUnifier TypeUnifier
    | USeq [TypeUnifier]
    | UVar Name
    deriving(Eq, Show)

ftv :: TypeUnifier -> Set.Set Name
ftv (UKind _) = Set.empty
ftv (UVar s) = Set.singleton s
ftv (UApp a b) = (ftv a) `Set.union` (ftv b)
ftv (USeq xs) = foldr (\x a -> (ftv x) `Set.union` a) Set.empty xs

type KindEnv = Map.Map String Kind

subst :: TypeUnifier -> KindEnv -> TypeUnifier
subst (UApp a b) env = UApp (subst a env) (subst b env)
subst (UVar s) env = case Map.lookup s env of
    Just x -> UKind x
    Nothing -> UVar s
subst (UKind x) env = UKind x
subst (USeq xs) env = USeq (map (flip subst env) xs)

type InferState = KindEnv

empty :: InferState
empty = Map.empty

type Infer = ExceptT (Set.Set KindError) (State InferState)

throw :: KindError -> Infer a
throw x = throwError (Set.singleton x)

infix 9 <~
(<~) :: Kind -> Kind -> Infer ()
(<~) KindAny KindSeq = pure ()
(<~) KindAny KindStar = pure ()
(<~) (KindFunc a b) (KindFunc x y) = a <~ x >> b <~ y
(<~) a b = if a == b then pure () else throw (UnificationError a b "kind")

infix 9 ~~
(~~) :: Kind -> Kind -> Infer Kind
KindAny ~~ KindSeq = pure KindSeq
KindAny ~~ KindStar = pure KindStar
KindSeq ~~ KindAny = pure KindSeq
KindStar ~~ KindAny = pure KindStar
(KindFunc a b) ~~ (KindFunc x y) = do
    f <- a ~~ x
    s <- b ~~ y
    pure (KindFunc f s)
a ~~ b = if a == b then pure a else throw (RebindError a b "kind")

simplify :: TypeUnifier -> Infer TypeUnifier
simplify (UApp a b) = do
    a' <- simplify a
    b' <- simplify b
    case (a', b') of
        ((UKind (KindFunc i o)), (UKind b)) -> do
            i <~ b
            pure (UKind o)
        ((UKind a), (UKind _)) -> throw (NotFunction a "application")
        (a, b) -> pure (UApp a b)
simplify (USeq xs) = do
    xs' <- mapM simplify xs
    let kinds = foldM (\a x -> case x of
            UKind k -> Just (k:a)
            _ -> Nothing) [] (reverse xs')
    case kinds of
        Just xs -> do
            mapM (\x -> KindAny <~ x) xs
            pure (UKind KindSeq)
        Nothing -> pure (USeq xs')
simplify (UVar s) = do
    env <- get
    pure $ case Map.lookup s env of
        Just k -> UKind k
        Nothing -> UVar s
simplify x = pure x

reduce :: TypeUnifier -> Infer TypeUnifier
reduce x = do
    env <- get
    simplify (subst x env)

extend :: String -> Kind -> Infer ()
extend s k = do
    env <- get
    case Map.lookup s env of
        Just x -> do
            x' <- x ~~ k
            put (Map.insert s x' env)
        Nothing -> put (Map.insert s k env)

guess :: TypeUnifier -> Maybe Kind
guess (UApp a _) = case guess a of
    Just (KindFunc i o) -> Just o
    _ -> Nothing
guess (UKind k) = Just k
guess _ = Nothing

-- TODO: refector `bind (UApp a b)` to reduce code duplication
-- seperate out (most) inference rules into distinct monads

bind :: TypeUnifier -> Kind -> Infer Int
bind (UApp a b) k = case guess a of
    Just x -> case x of                 -- K-ARG
        KindFunc i o -> do
            k <~ o
            an <- bind b i
            bn <- bind a x
            pure (an + bn)
        k -> throw (NotFunction k "bind")
    Nothing -> case guess b of
        Just x -> do                    -- K-APP
            an <- bind a (KindFunc x k)
            bn <- bind b x
            pure (an + bn)
        Nothing -> do
            an <- bindrec a
            bn <- bindrec b
            pure (an + bn)
{-
bind (UApp (UKind x) b) k = case x of   -- K-ARG
    KindFunc i o -> do
        k <~ o
        bind b i
    k -> throw (NotFunction k "bind")
bind (UApp a (UKind x)) k =             -- K-APP
    bind a (KindFunc x k)
-}
bind (USeq xs) k = do                   -- K-SEQ
    k <~ KindSeq
    xn <- mapM (\x -> bind x KindAny) xs
    pure (sum xn)
bind (UVar s) k = extend s k >> pure 1  -- K-VAR
bind (UKind k0) k = k <~ k0 >> pure 0
{-
bind (UApp a b) _ = do
    an <- bindrec a
    bn <- bindrec b
    pure (an + bn)
-}
bindrec :: TypeUnifier -> Infer Int
bindrec (UApp a b) = case guess a of
    Just x -> case x of                 -- K-ARG (again)
        KindFunc i o -> do
            an <- bind b i
            bn <- bind a x
            pure (an + bn)
        k -> throw (NotFunction k "bind")
    Nothing -> do
        an <- bindrec a
        bn <- bindrec b
        pure (an + bn)
bindrec (USeq xs) = do                  -- K-SEQ (again)
    xn <- mapM (\x -> bind x KindAny) xs
    pure (sum xn)
bindrec _ = pure 0

infer :: TypeUnifier -> Infer Kind
infer x = do
    n <- bindrec x
    if n == 0 then
        throw (InferError (ftv x))
    else do
        x' <- reduce x
        case x' of
            UKind k -> pure k
            _ -> infer x'

runKindInf :: Infer x -> InferState -> (Either (Set.Set KindError) x, InferState)
runKindInf m s = (runState (runExceptT m) s)