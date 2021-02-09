{-# LANGUAGE LambdaCase #-}

module Elaborate.Elaborate (elaborate) where

import Types.Pattern
import Types.Core
import Types.Module
import Types.Ident
import Types.Graph
import Types.Type
import Types.Prim

import qualified Types.Syntax as Syn

import Elaborate.Elaborator
import Elaborate.MatchComp

import Control.Monad.Errors
import Control.Monad
import Data.Maybe
import Control.Monad.RWS
import Control.Arrow

import Text.Parsec.Pos

import Data.List (intercalate)

import qualified Data.Map as Map

elaborate :: Int -> ModuleServer -> Env -> ModulePath -> [Syn.TopLevel] -> ErrorsResult ([ElabError], [ElabWarning]) (Core SourcePos, [GADT], [Eqtn], [Name], Int, [ElabWarning])
elaborate i ms e m tl =
    let env = (m, Map.fromList (termRenames e), Map.fromList (typeRenames e), Map.fromList (consTypes ms), Map.fromList (consRenames e))
        (res, s, w) = runElab (elabTL tl) env i
    in case res of
        Success (c, g, e, n) -> Success (c, g, e, n, s, w)
        FailWithResult r (c, g, e, n) -> FailWithResult (r, w) (c, g, e, n, s, w)
        Fail e -> Fail (e, w)

runElab :: Elaborator a -> ElabEnv -> ElabState -> (ErrorsResult [ElabError] a, ElabState, [ElabWarning])
runElab m = runRWS (runErrorsT m)

modul :: Elaborator ModulePath
modul = fmap (\(m,_,_,_,_) -> m) ask

freshen :: [Name] -> Elaborator [(Identifier, Identifier)]
freshen = mapM (\n -> fmap ((,) (LocalIdentifier n) . LocalIdentifier) (fork n))

withTerms :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTerms xs = local (\(m,a,b,c,d) -> (m,Map.fromList xs `mappend` a,b,c,d))

withTypes :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTypes xs = local (\(m,a,b,c,d) -> (m,a,Map.fromList xs `mappend` b,c,d))

withConsTys :: [(Identifier, Scheme)] -> Elaborator a -> Elaborator a
withConsTys xs = local (\(m,a,b,c,d) -> (m,a,b,Map.fromList xs `mappend` c,d))

withCons :: [(Identifier,Identifier)] -> Elaborator a -> Elaborator a
withCons xs = local (\(m,a,b,c,d) -> (m,a,b,c,Map.fromList xs `mappend` d))

lookupTerm :: Identifier -> Elaborator (Maybe Identifier)
lookupTerm i = do
    (_,m,_,_,_) <- ask
    pure (Map.lookup i m)

irrefuteLookupTerm :: SourcePos -> Identifier -> Elaborator Identifier
irrefuteLookupTerm p i = do
    (_,m,_,_,_) <- ask
    case Map.lookup i m of
        Just x -> pure x
        Nothing -> err i [UnboundTerm p i]

lookupType :: SourcePos -> Identifier -> Elaborator Identifier
lookupType t i = do
    (_,_,m,_,_) <- ask
    case Map.lookup i m of
        Just i' -> pure i'
        Nothing -> err i [UnboundType t i]

lookupCons :: Identifier -> Elaborator (Maybe (Identifier,Int))
lookupCons i = do
    (_,_,_,s,c) <- ask
    case Map.lookup i c of
        Just i' -> case Map.lookup i' s of
            Just sc -> pure (Just (i',aritySc sc))
            Nothing -> error "bees"
        Nothing -> pure Nothing

elabIdent :: SourcePos -> Identifier -> Elaborator (Core SourcePos)
elabIdent t i = do
    i' <- lookupTerm i
    c' <- lookupCons i
    case i' of
        Just i -> pure . Node t . Val $ Var i
        Nothing -> case c' of
            Just (c,a) -> do
                ns <- replicateM a fresh
                pure $ foldr (\n -> Node t . Lam (LocalIdentifier n)) (Node t . Cons c $ fmap (Var . LocalIdentifier) ns) ns
            Nothing -> err (Node t . Val $ Var i) [UnboundTerm t i]

elabPrim :: SourcePos -> Primop -> Elaborator (Core SourcePos)
elabPrim t p = do
    ns <- replicateM (arityOp p) fresh
    pure $ foldr (\n -> Node t . Lam (LocalIdentifier n)) (Node t . Prim p $ fmap (Var . LocalIdentifier) ns) ns

elabFun :: Syn.FunDef -> Elaborator (Core SourcePos)
elabFun (Syn.FunDef t mt _ args exp) =
    let lamexp = Syn.Lambda (Syn.Lam args exp)
        synexp = case mt of
            Just ty  -> Syn.Annot (Syn.Expl (Node t lamexp) ty)
            Nothing -> lamexp
    in elabExprNode t synexp

elabVal :: Syn.ValDef -> Elaborator (Core SourcePos)
elabVal (Syn.ValDef t (Just ty) _ exp) = elabExprNode t (Syn.Annot (Syn.Expl exp ty))
elabVal (Syn.ValDef t _ _ exp) = elabExpr exp

elabLam :: SourcePos -> Syn.Lam -> Elaborator (Core SourcePos)
elabLam t (Syn.Lam args exp) = do
    argm <- freshen args
    exp' <- withTerms argm (elabExpr exp)
    pure $ foldr (\(_,LocalIdentifier n) -> Node t . Lam (LocalIdentifier n)) exp' argm

elabPat :: SourcePattern -> Elaborator SourcePattern
elabPat (PatternCons t i ps) = do
    c' <- lookupCons i
    case c' of
        Just (c,_) -> do
            ps' <- mapM elabPat ps
            pure (PatternCons t c ps')
        Nothing -> case ps of
            [] -> pure (PatternVar t i)
            _ -> err (PatternVar t i) [PatternVarArgs t i ps]
elabPat x = pure x

elabMatch :: SourcePos -> Syn.Match -> Elaborator (Core SourcePos)
elabMatch t (Syn.Match e ps) = do
    n <- fresh
    e' <- elabExpr e
    rows <- forM ps $ \(p,e) -> do
            p' <- elabPat p
            pure ([p'], mempty, \m -> withTerms (Map.toList m) (elabExpr e))
    Node t . Let (LocalIdentifier n) e' <$> matchcomp t (rows, [LocalIdentifier n]) []
{-
elabMatch :: SourcePos -> Syn.Match -> Elaborator (Core SourcePos)
elabMatch t (Syn.Match e ps) = do
    n <- fresh
    e' <- elabExpr e
    exprCalls <- mapM (\(p,e) -> do
        fvexp <- freshen . Set.toList $ pvars p
        e' <- withTerms fvexp (elabExpr e)
        argns <- case fvexp of
            (_:_)   -> pure $ fmap (\(_,LocalIdentifier n) -> n) fvexp
            []      -> fmap (:[]) fresh
        let lamexp = foldr (\n -> Node t . Lam n) e' argns
        n <- fresh
        p' <- elabPat (Map.fromList $ fmap (\(LocalIdentifier a,LocalIdentifier b)->(a,b)) fvexp) p
        pure ((p',(n,argns,getTag e)),lamexp)) ps
    m <- matchComp t n (fmap fst exprCalls)
    pure . Node t . Fix (fmap (\((_,(n,_,_)),e) -> (LocalIdentifier n,e)) exprCalls) . Node t $ Let n e' m
-}
elabLet :: SourcePos -> Syn.Let -> Elaborator (Core SourcePos)
elabLet t (Syn.Let (d@(Syn.ValDef _ _ mn _):vs) e) = do
    v <- elabVal d
    case mn of
        Just n -> do
            f <- fork n
            Node t . Let (LocalIdentifier f) v <$> withTerms [(LocalIdentifier n,LocalIdentifier f)] (elabLet t (Syn.Let vs e))
        Nothing -> do
            f <- fresh
            Node t . Let (LocalIdentifier f) v <$> elabLet t (Syn.Let vs e)
elabLet _ (Syn.Let [] e) = elabExpr e

elabFix :: SourcePos -> Syn.Fix -> Elaborator (Core SourcePos)
elabFix t (Syn.Fix fs e) = do
    renames <- mapM (\d@(Syn.FunDef _ _ n _ _) -> do
        f <- fork n
        pure (LocalIdentifier n,LocalIdentifier f)) fs
    fs' <- withTerms renames $ mapM (\d@(Syn.FunDef p _ n _ _) -> do
        e <- elabFun d
        f <- irrefuteLookupTerm p (LocalIdentifier n)
        pure (f,e)) fs
    e' <- withTerms renames $ elabExpr e
    pure . Node t $ Fix fs' e'

elabTuple :: SourcePos -> [Syn.Expr] -> Elaborator (Core SourcePos)
elabTuple p es = internal [] p =<< mapM elabExpr es
    where
        internal :: [Value] -> SourcePos -> [Core SourcePos] -> Elaborator (Core SourcePos)
        internal vs tp (Node p (Val v):xs) = internal (vs ++ [v]) tp xs
        internal vs tp (x:xs) = do
            f <- fresh
            Node tp . Let (LocalIdentifier f) x <$> internal (vs ++ [Var (LocalIdentifier f)]) tp xs
        internal vs tp [] = pure . Node tp $ Tuple vs

elabType :: SourceType -> Elaborator Type
elabType (Node p (NamedType i)) = do
    i' <- lookupType p i
    pure . Node NoTag $ NamedType i'
elabType (App p a b) = do
    a' <- elabType a
    b' <- elabType b
    pure (App NoTag a' b')
elabType x = pure (untagType x)

elabAnnot :: SourcePos -> Syn.Annotation Syn.Expr -> Elaborator (Core SourcePos)
elabAnnot t (Syn.Expl e ty) = do
    sc <- elabType ty
    e' <- elabExpr e
    pure . Node t $ Annot e' sc

elabSelect :: SourcePos -> Int -> Syn.Expr -> Elaborator (Core SourcePos)
elabSelect p i e = do
    e' <- elabExpr e
    x <- fresh
    pure . Node p $ Let (LocalIdentifier x) e' (Node p $ Select i (Var $ LocalIdentifier x))

elabExprNode :: SourcePos -> Syn.ExprNode -> Elaborator (Core SourcePos)
elabExprNode t (Syn.Var i) = elabIdent t i
elabExprNode t (Syn.Annot a) = elabAnnot t a
elabExprNode t (Syn.LetIn l) = elabLet t l
elabExprNode t (Syn.FixIn f) = elabFix t f
elabExprNode t (Syn.Lambda l) = elabLam t l
elabExprNode t (Syn.Literal l) = pure . Node t . Val $ Lit l
elabExprNode t (Syn.Tuple ts) = elabTuple t ts
elabExprNode t (Syn.Select i e) = elabSelect t i e
elabExprNode t (Syn.Switch m) = elabMatch t m
elabExprNode t (Syn.Primop p) = elabPrim t p

elabExpr :: Syn.Expr -> Elaborator (Core SourcePos)
elabExpr (App t a b) = do
    a' <- elabExpr a
    b' <- elabExpr b
    pure $ App t a' b'
elabExpr (Node t a) = elabExprNode t a

getInd :: [Syn.TopLevel] -> [Syn.Data]
getInd (Syn.Data _ d:xs) = d:getInd xs
getInd (_:xs) = getInd xs
getInd [] = []

getEqtns :: [Syn.TopLevel] -> [(SourceType,SourceType)]
getEqtns (Syn.Family a b:xs) = (a,b):getEqtns xs
getEqtns (_:xs) = getEqtns xs
getEqtns [] = []

getTypes :: ModulePath -> [Syn.Data] -> [(Identifier, Identifier)]
getTypes m (Syn.Ind n _ _:xs) = (LocalIdentifier n,ExternalIdentifier m n):getTypes m xs
getTypes m [] = []

genGADT :: Syn.Data -> Elaborator GADT
genGADT (Syn.Ind n _ ns) = do
    m <- modul
    ns' <- mapM (\(Syn.Expl n t) -> fmap ((,) (ExternalIdentifier m n) . (\t -> Forall (ftv t) t)) (elabType t)) ns
    pure (GADT (ExternalIdentifier m n) (Node NoTag KindStar) ns')

genEqtn :: (SourceType,SourceType) -> Elaborator Eqtn
genEqtn (i,o) = do
    i <- elabType i
    o <- elabType o
    pure (Eqtn i o)

translateTL :: ModulePath -> [Name] -> [Syn.TopLevel] -> (Syn.Expr, [Name])
translateTL m ns (Syn.Group p fs:xs) =
    let names = fmap (\(Syn.FunDef _ _ n _ _) -> n) fs
        (exp, ns') = translateTL m (names++ns) xs
    in (Node p . Syn.FixIn $ Syn.Fix fs exp, ns')
translateTL m ns (Syn.Vals p vs:xs) =
    let names = fmap (\(Syn.ValDef _ _ n _) -> n) vs
        (exp, ns') = translateTL m (catMaybes names++ns) xs
    in (Node p . Syn.LetIn $ Syn.Let vs exp, ns')
translateTL m ns (_:xs) = translateTL m ns xs
translateTL m ns [] = (Node (initialPos (intercalate "." m)) $ Syn.Tuple (fmap (Node (initialPos (intercalate "." m)) . Syn.Var . LocalIdentifier) ns), ns)

collectCons :: ModulePath -> [GADT] -> ([(Identifier,Identifier)],[(Identifier, Scheme)])
collectCons m = mconcat . fmap (\(GADT _ _ ss) -> (fmap (\(ExternalIdentifier m i,_) -> (LocalIdentifier i, ExternalIdentifier m i)) ss,ss))

elabTL :: [Syn.TopLevel] -> Elaborator (Core SourcePos, [GADT], [Eqtn], [Name])
elabTL tl = do
    m <- modul
    let inds = getInd tl
    let eqtns = getEqtns tl
    let indtys = getTypes m inds
    let (expr, exports) = translateTL m [] tl
    withTypes indtys $ do
        gadts <- mapM genGADT inds
        eqtns <- mapM genEqtn eqtns
        let (terms, cons) = collectCons m gadts
        expr' <- withCons terms (withConsTys cons (elabExpr expr))
        pure (expr', gadts, eqtns, exports)

{-
collectTypes :: Module -> [Syn.TopLevel] -> [(Identifier,Identifier)]
collectTypes m (Syn.Data (Syn.Ind n _ _):xs) =
    (LocalIdentifier n,ExternalIdentifier m n):collectTypes m xs
collectTypes m (_:xs) = collectTypes m xs
collectTypes _ [] = []

collectTerms :: Module -> [Syn.TopLevel] -> [(Identifier,Identifier)]
collectTerms m (Syn.Group fs:xs) =
    fmap (\(Syn.FunDef _ _ i _ _) -> (LocalIdentifier i,ExternalIdentifier m i)) fs ++ collectTerms m xs
collectTerms m (Syn.Data (Syn.Ind _ _ ns):xs) =
    fmap (\(Syn.Expl a _) -> (LocalIdentifier a,ExternalIdentifier m a)) ns ++ collectTerms m xs
collectTerms _ [] = []

tlGen :: Type -> Scheme
tlGen t = Forall (ftv t) t

-- collect GADTs for environment purposes (and kindchecking, later?)
genGADTs :: [Syn.TopLevel] -> Elaborator [GADT]
genGADTs (Syn.Data (Syn.Ind n _ ns):xs) = do
    ns' <- mapM (\(Syn.Expl n t) -> fmap ((,) n . tlGen) (elabType t)) ns
    xs' <- genGADTs xs
    pure (GADT n ns':xs')
genGADTs (_:xs) = genGADTs xs
genGADTs [] = pure []

genFns :: [Syn.TopLevel] -> Elaborator [[(Identifier,Core SourcePos)]]
genFns (Syn.Group ds:xs) = do
    m <- modul
    grp <- forM ds $ \d@(Syn.FunDef _ _ n _ _) -> do
        let n' = ExternalIdentifier m n
        e' <- elabFun d
        pure (n',e')
    xs' <- genFns xs
    pure (grp:xs')
genFns (_:xs) = genFns xs
genFns [] = pure []

elabTL :: [Syn.TopLevel] -> Elaborator (Core SourcePos, [GADT])
elabTL xs = do
    m <- modul
    let t = initialPos (intercalate "." m)
    withTerms (collectTerms m xs) $ withTypes (collectTypes m xs) $ do
        gadts <- genGADTs xs
        fns <- withSchemes (collectCons m gadts) $ genFns xs
        let exp = foldr (\g -> Node t . Fix g) (Node t . Val $ Lit Unit) fns
        pure (exp, gadts)
-}