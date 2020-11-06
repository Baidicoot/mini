{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TypeCheck.Check where

import Types.Graph
import Types.Type
import Types.Ident
import Types.Core
import Types.Env

import Control.Monad.Reader
import Control.Monad.Errors
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Parsec.Pos

data Rigidity
    = R
    | W
    deriving(Eq, Show)

minR :: Rigidity -> Rigidity -> Rigidity
minR R R = R
minR _ _ = W

data TypeError
    = OccursFail SourcePos Name Type
    | UnifyFail SourcePos Type Type
    | MatchFail SourcePos Type Type
    | NotInScope SourcePos Identifier
    | ArityErr SourcePos Type
    | ImmedErr SourcePos Int Int
    deriving(Eq, Show)

fromUE :: UnifyError -> SourcePos -> TypeError
fromUE (OccursUE n t) p = OccursFail p n t
fromUE (UnifyUE a b) p = UnifyFail p a b
fromUE (MatchUE a b) p = MatchFail p a b

newtype Gamma = Gamma (Map.Map Identifier Scheme, Map.Map Identifier Rigidity, Map.Map Identifier Scheme, Set.Set Name)

instance Substitutable Gamma where
    apply s (Gamma (a, b, c, d)) = Gamma (apply s a, b, c, d)
    ftv (Gamma (a, b, c, d)) = ftv a `mappend` d

type Checker = ErrorsT [TypeError] (ReaderT Gamma (State (Int, Subst)))

liftUE :: SourcePos -> Either UnifyError a -> Checker a
liftUE _ (Right a) = pure a
liftUE p (Left e) = throw [fromUE e p]

getSubst :: Checker Subst
getSubst = fmap snd get

extSubst :: Subst -> Checker ()
extSubst s' = modify (\(n,s) -> (n,s' @@ s))

matches :: SourcePos -> Type -> Type -> Checker ()
matches t a b = do
    s  <- getSubst
    s' <- revert mempty $ liftUE t (match (apply s a) (apply s b))
    extSubst s'

unify :: SourcePos -> Type -> Type -> Checker ()
unify t a b = do
    s  <- getSubst
    s' <- revert mempty $ liftUE t (mgu a b)
    extSubst s'

immedMatch :: SourcePos -> [a] -> [b] -> Checker ()
immedMatch t a b =
    if length a /= length b then
        report [ImmedErr t (length a) (length b)]
    else
        pure ()

newest :: Substitutable t => t -> Checker t
newest t = do
    s <- getSubst
    pure (apply s t)

fresh :: Checker Name
fresh = do
    (n,s) <- get
    put (n+1,s)
    pure ('t':show n)

freshTV :: Checker Type
freshTV = do
    f <- fresh
    pure (Node NoTag (TypeVar f))

generalize :: Type -> Checker Scheme
generalize t = do
    g <- ask
    s <- getSubst
    let t' = apply s t
    let a = ftv t' `Set.difference` ftv g
    pure $ Forall a t'

instantiate :: Scheme -> Checker Type
instantiate (Forall a t) = do
    theta <- mapM (flip fmap freshTV . (,)) (Set.toList a)
    pure (apply (Map.fromList theta) t)

lookupLocal :: Identifier -> SourcePos -> Checker Scheme
lookupLocal i t = do
    Gamma (a,_,_,_) <- ask
    case Map.lookup i a of
        Just sc -> pure sc
        Nothing -> do
            f <- fresh
            err (Forall (Set.singleton f) . Node NoTag $ TypeVar f) [NotInScope t i]

lookupCons :: Identifier -> SourcePos -> Checker Scheme
lookupCons i t = do
    Gamma (_,_,a,_) <- ask
    case Map.lookup i a of
        Just sc -> pure sc
        Nothing -> do
            f <- fresh
            err (Forall (Set.singleton f) . Node NoTag $ TypeVar f) [NotInScope t i]

lookupRigidity :: Identifier -> Checker Rigidity
lookupRigidity i = do
    Gamma (_,a,_,_) <- ask
    case Map.lookup i a of
        Just r  -> pure r
        Nothing -> pure W

withTypes :: [(Identifier, Scheme)] -> Checker a -> Checker a
withTypes ts = local (\(Gamma (a,b,c,d)) -> Gamma (Map.fromList ts `mappend` a,b,c,d))

withFV :: Set.Set Name -> Checker a -> Checker a
withFV v = local (\(Gamma (a,b,c,d)) -> Gamma (a,b,c,d `mappend` v))

withRigidity :: [(Identifier, Rigidity)] -> Checker a -> Checker a
withRigidity rs = local (\(Gamma (a,b,c,d)) -> Gamma (a,Map.fromList rs `mappend` b,c,d))

withEnv :: [(Identifier, Rigidity, Scheme)] -> Checker a -> Checker a
withEnv rts = withRigidity (fmap (\(a,b,c)->(a,b)) rts) . withTypes (fmap (\(a,b,c)->(a,c)) rts)

splitArr :: SourcePos -> Type -> Checker (Type, Type)
splitArr p = newest >=> internal
    where
    internal (App _ (App _ (Node _ FunctionType) a) b) = pure (a, b)
    internal t = do
        t1 <- freshTV
        t2 <- freshTV
        t' <- newest t
        report [ArityErr p t']
        pure (t1, t2)

class Inferable t w | t -> w where
    -- bidirectional typechecking augmented with inferred substitutions
    check :: Rigidity -> t -> Type -> Checker w
    infer :: Rigidity -> t -> Checker (w, Type)
    rigid :: t -> Checker Rigidity

instance Inferable (Core SourcePos) (Core Type) where
    -- VAR
    infer m (Node p (Val (Var i))) = do
        s <- lookupLocal i p
        t <- instantiate s
        pure (Node t . Val $ Var i, t)
    -- LIT
    infer m (Node p (Val (Lit l))) = pure (Node (litTy l) . Val $ Lit l, litTy l)
    -- APP
    infer m (App p f x) = do
        (f',ft) <- infer W f
        (t1,t2) <- splitArr p ft
        x'      <- check W x t1
        pure (App t2 f' x', t2)
    -- ABS-INFER
    infer m (Node p (Lam x t)) = do
        t1  <- freshTV
        t2  <- freshTV
        t'  <- withTypes [(LocalIdentifier x, Forall mempty t1)] (check m t t2)
        pure (Node (t1 --> t2) (Lam x t'), t1 --> t2)
    -- ANN
    infer m (Node p (Annot x t)) = do
        sigma <- generalize t
        tau   <- instantiate sigma
        x'    <- withFV (quantified sigma) (check m x tau)
        pure (x', tau)
    -- PRIM
    infer m (Node p (Prim op vs)) = do
        let t1 = argTys (opTy op)
        let t2 = resTy  (opTy op)
        immedMatch p vs t1
        let vs' = fmap (Node p . Val) vs
        mapM_ (uncurry (check m :: (Core SourcePos) -> Type -> Checker (Core Type))) (zip vs' t1)
        pure (Node t2 (Prim op vs), t2)
    -- CONS
    infer m (Node p (Cons i vs)) = do
        sc <- lookupCons i p
        ty <- instantiate sc
        let t1 = argTys ty
        let t2 = resTy  ty
        immedMatch p vs t1
        let vs' = fmap (Node p . Val) vs
        mapM_ (uncurry (check m :: (Core SourcePos) -> Type -> Checker (Core Type))) (zip vs' t1)
        pure (Node t2 (Cons i vs), t2)
    -- LET
    infer m (Node p (Let x u t)) = do
        m1      <- rigid u
        (u',t1) <- infer W u
        s1      <- generalize t1
        (t',t2) <- withEnv [(LocalIdentifier x,m1,s1)] (infer m t)
        pure (Node t2 (Let x u' t'), t2)
    -- FIX
    infer m (Node p (Fix fs t)) = do
        imtxs <- mapM (\case
            (i,Node p (Annot x t)) -> do
                sc <- generalize t
                pure (i,R,sc,x)
            (i,x) -> do
                m <- rigid x
                t <- freshTV
                pure (i,m,Forall mempty t,x)) fs
        let env = fmap (\(a,b,c,d)->(a,b,c)) imtxs
        imtxs' <- withEnv env . flip mapM imtxs $ \(i,m,sc,x) -> do
            t       <- instantiate sc
            x'      <- check W x t
            sc'     <- generalize t
            pure (i,m,sc',x')
        let env' = fmap (\(a,b,c,d)->(a,b,c)) imtxs'
        let fs' = fmap (\(a,b,c,d)->(a,d)) imtxs'
        (t',t2) <- withEnv env' (infer m t)
        pure (Node t2 (Fix fs' t'), t2)
    
    -- ABS-CHECK
    check m (Node p (Lam x t)) ft = do
        (t1,t2) <- splitArr p ft
        t' <- withEnv [(LocalIdentifier x,m,Forall mempty t1)] (check m t t2)
        pure (Node (t1 --> t2) (Lam x t'))
    -- CHECK-INFER
    check m t t1 = do
        (t',t2) <- infer m t
        matches (getTag t) t2 t1
        pure t'
    
    -- SCR-VAR
    rigid (Node p (Val (Var i))) = lookupRigidity i
    -- SCR-APP
    rigid (App p a b) = do
        m1 <- rigid a
        m2 <- rigid b
        pure (minR m1 m2)
    -- SCR-LET
    rigid (Node p (Let x u t)) = do
        m1 <- rigid u
        withRigidity [(LocalIdentifier x, m1)] (rigid t)
    -- SCR-ABS
    rigid (Node p (Lam x t)) = rigid t
    -- SCR-ANN
    rigid (Node p (Annot x t)) = pure R
    -- SCR-OTHER
    rigid t = pure W