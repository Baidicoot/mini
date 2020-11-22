{-# LANGUAGE LambdaCase #-}

module Elaborate.Elaborate (elaborate) where

import Types.Pattern
import Types.Core
import Types.Env
import Types.Ident
import Types.Graph
import Types.Type
import Types.Prim

import qualified Types.Syntax as Syn

import Elaborate.Elaborator
import Elaborate.MatchComp

import Control.Monad.Errors
import Control.Monad
import Control.Monad.RWS
import Control.Arrow

import Text.Parsec.Pos

import Data.List (intercalate)

import qualified Data.Map as Map

elaborate :: [(ModuleExports,ImportAction)] -> Int -> Module -> Env -> [Syn.TopLevel] -> ErrorsResult ([ElabError], [ElabWarning]) (Core SourcePos, ModuleExports, Int, [ElabWarning])
elaborate imports i m e tl =
    let env = (m, termRenames e, typeRenames e, fmap (\(a,b,c)->b) (consInfo e))
        (res, s, w) = runElab (elabTL imports tl) env i
    in case res of
        Success (c, g) -> Success (c, g, s, w)
        FailWithResult e (c, g) -> FailWithResult (e, w) (c, g, s, w)
        Fail e -> Fail (e, w)

runElab :: Elaborator a -> ElabEnv -> ElabState -> (ErrorsResult [ElabError] a, ElabState, [ElabWarning])
runElab m = runRWS (runErrorsT m)

modul :: Elaborator Module
modul = fmap (\(m,_,_,_) -> m) ask

freshen :: [Name] -> Elaborator [(Identifier, Identifier)]
freshen = mapM (\n -> fmap ((,) (LocalIdentifier n) . LocalIdentifier) (fork n))

withTerms :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTerms xs = local (\(m,a,b,c) -> (m,Map.fromList xs `mappend` a,b,c))

withTypes :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTypes xs = local (\(m,a,b,c) -> (m,a,Map.fromList xs `mappend` b,c))

withCons :: [(Identifier, Scheme)] -> Elaborator a -> Elaborator a
withCons xs = local (\(m,a,b,c) -> (m,a,b,Map.fromList xs `mappend` c))

lookupTerm :: SourcePos -> Identifier -> Elaborator Identifier
lookupTerm t i = do
    (_,m,_,_) <- ask
    case Map.lookup i m of
        Just i' -> pure i'
        Nothing -> err i [UnboundTerm t i]

lookupType :: SourcePos -> Identifier -> Elaborator Identifier
lookupType t i = do
    (_,_,m,_) <- ask
    case Map.lookup i m of
        Just i' -> pure i'
        Nothing -> err i [UnboundType t i]

lookupCons :: Identifier -> Elaborator (Maybe Scheme)
lookupCons i = do
    (_,_,_,c) <- ask
    pure (Map.lookup i c)

elabIdent :: SourcePos -> Identifier -> Elaborator (Core SourcePos)
elabIdent t i = do
    i' <- lookupTerm t i
    sc <- lookupCons i'
    case sc of
        Just sc -> do
            ns <- replicateM (aritySc sc) fresh
            pure $ foldr (\n -> Node t . Lam n) (Node t . Cons i' $ fmap (Var . LocalIdentifier) ns) ns
        Nothing -> pure . Node t . Val $ Var i'

elabPrim :: SourcePos -> Primop -> Elaborator (Core SourcePos)
elabPrim t p = do
    ns <- replicateM (arityOp p) fresh
    pure $ foldr (\n -> Node t . Lam n) (Node t . Prim p $ fmap (Var . LocalIdentifier) ns) ns

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
    pure $ foldr (\(_,LocalIdentifier n) -> Node t . Lam n) exp' argm

elabPat :: SourcePattern -> Elaborator SourcePattern
elabPat (PatternCons t i ps) = do
    i' <- lookupTerm t i
    ps' <- mapM elabPat ps
    pure (PatternCons t i' ps')
elabPat x = pure x

elabMatch :: SourcePos -> Syn.Match -> Elaborator (Core SourcePos)
elabMatch t (Syn.Match e ps) = do
    n <- fresh
    e' <- elabExpr e
    rows <- forM ps $ \(p,e) -> do
            p' <- elabPat p
            pure ([p'], mempty, \m -> withTerms ((LocalIdentifier *** LocalIdentifier) <$> Map.toList m) (elabExpr e))
    Node t . Let n e' <$> matchcomp t (rows, [n]) []
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
elabLet t (Syn.Let vs e) = do
    nvs <- mapM (\d@(Syn.ValDef _ _ n _) -> do
        e <- elabVal d
        f <- fork n
        pure ((LocalIdentifier n,LocalIdentifier f),e)) vs
    e' <- withTerms (fmap fst nvs) (elabExpr e)
    pure $ foldr (\((_,LocalIdentifier n),e) -> Node t . Let n e) e' nvs

elabFix :: SourcePos -> Syn.Fix -> Elaborator (Core SourcePos)
elabFix t (Syn.Fix fs e) = do
    nfs <- mapM (\d@(Syn.FunDef _ _ n _ _) -> do
        e <- elabFun d
        f <- fork n
        pure ((LocalIdentifier n,LocalIdentifier f),e)) fs
    e' <- withTerms (fmap fst nfs) (elabExpr e)
    pure . Node t $ Fix (fmap (\((_,n),e) -> (n,e)) nfs) e'

elabTuple :: SourcePos -> [Syn.Expr] -> Elaborator (Core SourcePos)
elabTuple p es = internal [] p =<< mapM elabExpr es
    where
        internal :: [Value] -> SourcePos -> [Core SourcePos] -> Elaborator (Core SourcePos)
        internal vs tp (Node p (Val v):xs) = internal (vs ++ [v]) tp xs
        internal vs tp (x:xs) = do
            f <- fresh
            Node tp . Let f x <$> internal (vs ++ [Var (LocalIdentifier f)]) tp xs
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
    pure . Node p $ Let x e' (Node p $ Select i (Var $ LocalIdentifier x))

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

getTypes :: Module -> [Syn.Data] -> [(Identifier, Identifier)]
getTypes m (Syn.Ind n _ _:xs) = (LocalIdentifier n,ExternalIdentifier m n):getTypes m xs
getTypes m [] = []

genGADT :: Syn.Data -> Elaborator GADT
genGADT (Syn.Ind n _ ns) = do
    ns' <- mapM (\(Syn.Expl n t) -> fmap ((,) n . (\t -> Forall (ftv t) t)) (elabType t)) ns
    pure (GADT n ns')

translateTL :: Module -> [Name] -> [Syn.TopLevel] -> (Syn.Expr, [Name])
translateTL m ns (Syn.Group p fs:xs) =
    let names = fmap (\(Syn.FunDef _ _ n _ _) -> n) fs
        (exp, ns') = translateTL m (names++ns) xs
    in (Node p . Syn.FixIn $ Syn.Fix fs exp, ns')
translateTL m ns (Syn.Vals p vs:xs) =
    let names = fmap (\(Syn.ValDef _ _ n _) -> n) vs
        (exp, ns') = translateTL m (names++ns) xs
    in (Node p . Syn.LetIn $ Syn.Let vs exp, ns')
translateTL m ns (_:xs) = translateTL m ns xs
translateTL m ns [] = (Node (initialPos (intercalate "." m)) $ Syn.Tuple (fmap (Node (initialPos (intercalate "." m)) . Syn.Var . LocalIdentifier) ns), ns)

collectCons :: Module -> [GADT] -> ([(Identifier,Identifier)],[(Identifier, Scheme)])
collectCons m = mconcat . fmap (\(GADT _ ss) -> (fmap (\(i,_) -> (LocalIdentifier i, ExternalIdentifier m i)) ss,fmap (first (ExternalIdentifier m)) ss))

elabTL :: [(ModuleExports,ImportAction)] -> [Syn.TopLevel] -> Elaborator (Core SourcePos, ModuleExports)
elabTL imports tl = do
    m <- modul
    let inds = getInd tl
    let indtys = getTypes m inds
    let (expr, exports) = translateTL m [] tl
    withTypes indtys $ do
        gadts <- mapM genGADT inds
        let (terms, cons) = collectCons m gadts
        expr' <- withTerms terms $ withCons cons (elabExpr expr)
        pure (expr', mempty {moduleMod = m, termNames = exports} `mappend` includeGADTs gadts)

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