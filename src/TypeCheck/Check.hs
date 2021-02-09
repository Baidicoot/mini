{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TypeCheck.Check where

import Types.Graph
import Types.Type
import Types.Ident
import Types.Core
import Types.Module
import Types.Prim

import Control.Monad.Reader
import Control.Monad.Errors
import Control.Monad.State
import Control.Arrow

import qualified Data.Map as Map
import qualified Data.Set as Set

import TypeCheck.Meta
import Error.Error
import Text.Parsec.Pos

data Rigidity
    = R
    | W
    deriving(Eq, Show)

minR :: Rigidity -> Rigidity -> Rigidity
minR R R = R
minR _ _ = W

data UnifyAction
    = UnifyAct Type Type
    | MatchAct Type Type
    deriving(Eq)

instance Show UnifyAction where
    show (UnifyAct t1 t2) = "unifying '" ++ show t1 ++ "' with '" ++ show t2 ++ "'"
    show (MatchAct t1 t2) = "matching '" ++ show t1 ++ "' with '" ++ show t2 ++ "'"

data TypeError
    = UnifyError SourcePos UnifyError UnifyAction
    | NotInScope SourcePos Identifier
    | ImmedErr SourcePos Int Int
    | SelectErr SourcePos Type Int
    | Debug SourcePos String
    deriving(Eq, Show)

instance RenderableError TypeError where
    errType _ = "type error"

    errPos (UnifyError p _ _) = p
    errPos (NotInScope p _) = p
    errPos (ImmedErr p _ _) = p
    errPos (SelectErr p _ _) = p
    errPos (Debug p _) = p

    errCont (UnifyError _ e a) =
        [ show e
        , "while " ++ show a ]
    errCont (NotInScope _ i) = ["the variable '" ++ show i ++ "' is unbound"]
    errCont (ImmedErr _ i j) = ["expected " ++ show i ++ " arguments, but got " ++ show j]
    errCont (SelectErr _ t i) = ["cannot extract the " ++ show i ++ "th element from " ++ show t]
    errCont (Debug _ s) = [s]

newtype Gamma = Gamma
    ( Map.Map Identifier Scheme
    , Map.Map Identifier Rigidity
    , Map.Map Identifier Scheme
    , Set.Set Name
    , [Eqtn])

instance Substitutable Gamma where
    apply s (Gamma (a, b, c, d, e)) = Gamma (apply s a, b, c, d, e)
    ftv (Gamma (a, _, _, d, _)) = ftv a `mappend` d

type Checker = ErrorsT [TypeError] (ReaderT Gamma (State (Int, Subst)))

liftUE :: SourcePos -> UnifyAction -> a -> Either UnifyError a -> Checker a
liftUE _ _ _ (Right x) = pure x
liftUE t ac a (Left e) = err a [UnifyError t e ac]

typecheck :: Int -> ModuleServer -> Core SourcePos -> ErrorsResult [TypeError] (Core Type, [Scheme], Int)
typecheck i e c =
    let env = Gamma (Map.fromList (termTypes e), mempty, Map.fromList (consTypes e), mempty, eqtns e)
        (r,i',s) = runChecker
            env
            (i,mempty)
            (infer W c)
    in case r of
        Success (c,t) -> let c' = apply s c in Success (c', gatherExterns c', i')
        FailWithResult e (c,t) -> let c' = apply s c in FailWithResult e (c', gatherExterns c', i')
        Fail e -> Fail e

runChecker :: Gamma -> (Int, Subst) -> Checker a -> (ErrorsResult [TypeError] a, Int, Subst)
runChecker e s m =
    let (a,(b,c)) = runState (runReaderT (runErrorsT m) e) s
    in (a,b,c)

getSubst :: Checker Subst
getSubst = gets snd

extSubst :: Subst -> Checker ()
extSubst s' = modify (second (s' @@))

matches :: SourcePos -> Type -> Type -> Checker ()
matches t a b = do
    Gamma (_,_,_,r,e) <- ask
    s  <- getSubst
    s' <- liftUE t (MatchAct (apply s a) (apply s b)) mempty (match e r (apply s a) (apply s b))
    extSubst s'

unify :: SourcePos -> Type -> Type -> Checker ()
unify t a b = do
    Gamma (_,_,_,_,e) <- ask
    s  <- getSubst
    s' <- liftUE t (UnifyAct (apply s a) (apply s b)) mempty (mgu e (apply s a) (apply s b))
    extSubst s'

unifier :: SourcePos -> Type -> Type -> Checker Subst
unifier t a b = do
    Gamma (_,_,_,_,e) <- ask
    s <- getSubst
    s' <- liftUE t (UnifyAct (apply s a) (apply s b)) mempty (mgu e (apply s a) (apply s b))
    pure (s @@ s')

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
freshTV = Node NoTag . TypeVar <$> fresh

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
    Gamma (a,_,_,_,_) <- ask
    case Map.lookup i a of
        Just sc -> newest sc
        Nothing -> do
            f <- fresh
            err (Forall (Set.singleton f) . Node NoTag $ TypeVar f) [NotInScope t i]

lookupCons :: Identifier -> SourcePos -> Checker Scheme
lookupCons i t = do
    Gamma (_,_,a,_,_) <- ask
    case Map.lookup i a of
        Just sc -> pure sc
        Nothing -> do
            f <- fresh
            err (Forall (Set.singleton f) . Node NoTag $ TypeVar f) [NotInScope t i]

lookupRigidity :: Identifier -> Checker Rigidity
lookupRigidity i = do
    Gamma (_,a,_,_,_) <- ask
    case Map.lookup i a of
        Just r  -> pure r
        Nothing -> pure W

withTypes :: [(Identifier, Scheme)] -> Checker a -> Checker a
withTypes ts = local (\(Gamma (a,b,c,d,e)) -> Gamma (Map.fromList ts `mappend` a,b,c,d,e))

withFV :: Set.Set Name -> Checker a -> Checker a
withFV v = local (\(Gamma (a,b,c,d,e)) -> Gamma (a,b,c,d `mappend` v,e))

withRigidity :: [(Identifier, Rigidity)] -> Checker a -> Checker a
withRigidity rs = local (\(Gamma (a,b,c,d,e)) -> Gamma (a,Map.fromList rs `mappend` b,c,d,e))

withEnv :: [(Identifier, Rigidity, Scheme)] -> Checker a -> Checker a
withEnv rts = withRigidity (fmap (\(a,b,c)->(a,b)) rts) . withTypes (fmap (\(a,b,c)->(a,c)) rts)

splitArr :: SourcePos -> Type -> Checker (Type, Type)
splitArr p t = do
    t1 <- freshTV
    t2 <- freshTV
    matches p (t1 --> t2) t
    t1' <- newest t1
    t2' <- newest t2
    pure (t1',t2')

class Inferable t w | t -> w where
    -- bidirectional typechecking augmented with inferred substitutions
    check :: Rigidity -> t -> Type -> Checker w
    infer :: Rigidity -> t -> Checker (w, Type)
    rigid :: t -> Checker Rigidity

infFixDefs :: [(Identifier, Core SourcePos)] -> Checker [(Identifier,Rigidity,Scheme,Core Type)]
infFixDefs fs = do
    fenv <- forM fs $ \case
        (i,Node _ (Annot x t)) -> do
            sc <- generalize t
            pure (i,R,sc,x,True)
        (i,x) -> do
            m <- rigid x
            t <- freshTV
            pure (i,m,unqualified t,x,False)
    withEnv (fmap (\(a,b,c,_,_)->(a,b,c)) fenv) . forM fenv $ \case
        (i,_,sc@(Forall q t),x,True) -> do
            x' <- withFV q $ check R x t
            pure (i,R,sc,x')
        (i,m,Forall _ t,x,False) -> do
            x' <- check m x t
            sc <- generalize t
            pure (i,m,sc,x')

infLetDef :: Identifier -> Core SourcePos -> Checker (Identifier,Rigidity,Scheme,Core Type)
infLetDef x u = do
    m1 <- rigid u
    (u',t1) <- infer W u
    s1 <- generalize t1
    pure (x,m1,s1,u')

instance Inferable (Core SourcePos) (Core Type) where
    -- VAR
    infer m (Node p (Val (Var i))) = do
        s <- lookupLocal i p
        t <- instantiate s
        pure (Node t . Val $ Var i, t)
    -- LIT
    infer m (Node p (Val (Lit l))) = do
        let s = litTy l
        t <- instantiate s
        pure (Node t . Val $ Lit l, t)
    -- ERROR
    infer m (Node p (Error s)) = do
        t <- freshTV
        pure (Node t (Error s), t)
    -- APP
    infer m (App p f x) = do
        (f',ft) <- infer W f
        (t1,t2) <- splitArr p ft
        x'      <- check W x t1
        pure (App t2 f' x', t2)
    -- ABS-INFER
    infer m (Node p (Lam x t)) = do
        t1 <- freshTV
        (t',t2) <- withTypes [(x, Forall mempty t1)] (infer m t)
        pure (Node (t1 --> t2) (Lam x t'), t1 --> t2)
    -- ANN
    infer m (Node p (Annot x t)) = do
        (Forall q t)    <- generalize t
        x'              <- withFV q (check R x t)
        tau             <- instantiate (Forall q t)
        pure (x', tau)
    -- PRIM
    infer m (Node p (Prim op vs)) = do
        ty <- instantiate =<< generalize (opTy op)
        let t1 = argTys ty
        let t2 = resTy ty
        immedMatch p vs t1
        let vs' = fmap (Node p . Val) vs
        mapM_ (uncurry (check m :: Core SourcePos -> Type -> Checker (Core Type))) (zip vs' t1)
        pure (Node t2 (Prim op vs), t2)
    -- CONS
    infer m (Node p (Cons i vs)) = do
        ty <- instantiate =<< lookupCons i p
        let t1 = argTys ty
        let t2 = resTy ty
        immedMatch p vs t1
        let vs' = fmap (Node p . Val) vs
        mapM_ (uncurry (check m :: Core SourcePos -> Type -> Checker (Core Type))) (zip vs' t1)
        pure (Node t2 (Cons i vs), t2)
    -- TUPLE
    infer m (Node p (Tuple vs)) = do
        let vs' = fmap (Node p . Val) vs :: [Core SourcePos]
        (_,ts') <- mapAndUnzipM (infer m) vs'
        pure (Node (Node NoTag (Product ts')) (Tuple vs), Node NoTag (Product ts'))
    infer m (Node p (Select i v)) = do
        vt <- newest . snd =<< (infer m :: Core SourcePos -> Checker (Core Type,Type)) (Node p (Val v))
        et <- case vt of
            Node _ (Product ts) | i < length ts -> pure $ ts !! i
            _ -> do
                t <- freshTV
                err t [SelectErr p vt i]
        pure (Node et (Select i v), et)
    -- LET-INFER
    infer m (Node p (Let x u t)) = do
        (x',m1,s1,u') <- infLetDef x u
        (t',t2) <- withEnv [(x',m1,s1)] (infer m t)
        pure (Node t2 (Let x u' t'), t2)
    -- FIX-INFER
    infer m n@(Node p (Fix fs t)) = do
        imtxs <- infFixDefs fs
        let tenv = fmap (\(a,b,c,d)->(a,b,c)) imtxs
        let fs' = fmap (\(a,b,c,d)->(a,d)) imtxs
        (t',t2) <- withEnv tenv (infer m t)
        pure (Node t2 (Fix fs' t'), t2)
    -- MATCH-INFER
    infer m (Node p (Match _ n cs)) = do
        tp <- instantiate =<< lookupLocal n p
        tt <- freshTV
        cs' <- mapM (\case
            -- PCONS-INFER
            (ConsPattern c v,s,t) -> do
                p <- instantiate =<< lookupCons c s
                let pargs = argTys p
                let pres = resTy p
                immedMatch s pargs v
                unify s pres tp
                let env = (\(v,t)->(LocalIdentifier v,W,unqualified t)) <$> zip v pargs
                (t',tt') <- withEnv env (infer m t)
                unify s tt tt'
                pure (ConsPattern c v,tt',t')
            -- PLIT-INFER
            (LiteralPattern l,s,t) -> do
                lt <- instantiate (litTy l)
                unify s lt tp
                (t',tt') <- infer m t
                unify s tt tt'
                pure (LiteralPattern l,tt',t')
            -- PWILD-INFER
            (WildcardPattern,s,t) -> do
                (t',tt') <- infer m t
                unify s tt tt'
                pure (WildcardPattern,tt',t')) cs
        pure (Node tt (Match (Just tp,p) n cs'), tt)

    -- ABS-CHECK
    check m (Node p (Lam x t)) s = do
        (t1,t2) <- splitArr p s
        t' <- withEnv [(x,m,Forall mempty t1)] (check m t t2)
        pure (Node (t1 --> t2) (Lam x t'))
    -- LET-CHECK
    check m (Node p (Let x u t)) t2 = do
        (x',m1,s1,u') <- infLetDef x u
        t' <- withEnv [(x',m1,s1)] (check m t t2)
        pure (Node t2 (Let x u' t'))
    -- FIX-CHECK
    check m (Node p (Fix fs t)) t1 = do
        imtxs <- infFixDefs fs
        let tenv = fmap (\(a,b,c,d)->(a,b,c)) imtxs
        let fs' = fmap (\(a,b,c,d)->(a,d)) imtxs
        t' <- withEnv tenv (check m t t1)
        pure (Node t1 (Fix fs' t'))
    -- MATCH-CHECK
    check m (Node p (Match _ n cs)) ts = do
        m1 <- lookupRigidity n
        tp <- instantiate =<< lookupLocal n p
        cs' <- mapM (pcon m1 m tp ts) cs
        pure (Node ts (Match (Just tp,p) n cs'))
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
        withRigidity [(x, m1)] (rigid t)
    -- SCR-ABS
    rigid (Node p (Lam x t)) = rigid t
    -- SCR-ANN
    rigid (Node p (Annot x t)) = pure R
    -- SCR-OTHER
    rigid t = pure W

-- need to check matching of types: seems to match expression type to pattern type
pcon :: Rigidity -> Rigidity -> Type -> Type
    -> (PatternBinding, SourcePos, Core SourcePos)
    -> Checker (PatternBinding, Type, Core Type)
-- PCON-W
pcon W m tp st (ConsPattern c v,s,t) = do
    p <- instantiate =<< lookupCons c s
    let pargs = argTys p
    let pres = resTy p
    let locally = ftv p `Set.difference` ftv pres
    immedMatch s pargs v
    matches s pres tp
    let env = (\(v,t)->(LocalIdentifier v,W,unqualified t)) <$> zip v pargs
    t' <- withFV locally $ withEnv env (check m t st)
    pure (ConsPattern c v,pres,t')
-- PCON-R
pcon R m tp st (ConsPattern c v,s,t) = do
    p <- instantiate =<< lookupCons c s
    let pargs = argTys p
    let pres = resTy p
    let locally = ftv p `Set.difference` ftv pres
    immedMatch s pargs v
    theta <- unifier s pres tp
    let env = (\(v,t)->(LocalIdentifier v,R,unqualified (apply theta t))) <$> zip v pargs
    let st' = apply theta st
    t' <- withFV locally $ withEnv env (check m t st')
    pure (ConsPattern c v,pres,t')
-- PWILD
pcon _ m tp st (WildcardPattern,s,t) = do
    t' <- check m t st
    pure (WildcardPattern,tp,t')
-- PLIT
pcon _ m tp st (LiteralPattern l,s,t) = do
    lt <- instantiate (litTy l)
    matches s lt tp
    t' <- check m t st
    pure (LiteralPattern l,lt,t')