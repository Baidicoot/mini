module Elaborate.Elaborate where

import Types.Pattern
import Types.Core

import qualified Types.Syntax as Syn

import Elaborate.MatchComp

import Control.Monad.Errors
import Control.Monad
import Control.Monad.RWS
import Control.Arrow

import Text.Parsec.Pos

import Data.List (intercalate)

import qualified Data.Map as Map
import qualified Data.Set as Set

data ElabError
    = UnboundTerm SourcePos Identifier
    | UnboundType SourcePos Identifier
    | Conflicting SourcePos Name
    deriving(Show)

data ElabWarning
    = MatchWarning SourcePos [MatchWarning SourcePos]
    deriving(Show)

type ElabEnv = (Module, Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier Scheme)
type ElabState = Int
type PolyElaborator = ErrorsT [ElabError] (RWS ElabEnv [ElabWarning] ElabState)

runElab :: PolyElaborator a -> ElabEnv -> ElabState -> (Either [ElabError] a, ElabState, [ElabWarning])
runElab m r s = runRWS (runErrorsT m) r s

matchComp :: SourcePos -> Name -> [(Pattern tag, Call tag)] -> Elaborator (Core SourcePos)
matchComp t n ps = do
    n <- get
    let (c,n',w) = matchcomp n t ps
    case w of
        (_:_)   -> tell [MatchWarning t w]
        []      -> pure ()
    put n'
    pure c

mod :: Elaborator Module
mod = fmap (\(m,_,_,_) -> m) ask

fresh :: Elaborator Name
fresh = do
    n <- get
    put (n+1)
    pure ('v':show n)

freshen :: [Name] -> Elaborator [(Identifier, Identifier)]
freshen = mapM (\n -> fmap ((,) LocalIdentifier n . LocalIdentifier) fresh)

withTerms :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTerms xs = local (\(m,a,b,c) -> (m,Map.fromList xs `mappend` a,b,c))

withTypes :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTypes xs = local (\(m,a,b,c) -> (m,a,Map.fromList xs `mappend` b,c))

lookupTerm :: SourcePos -> Identifier -> Elaborator Identifier
lookupTerm t i = do
    (_,m,_,_) <- ask
    case Map.lookup i m of
        Just i' -> pure i'
        Nothing -> err [UnboundTerm t i']

lookupType :: SourcePos -> Identifier -> Elaborator Identifier
lookupType t i = do
    (_,_,m,_) <- ask
    case Map.lookup i m of
        Just i' -> pure i'
        Nothing -> err [UnboundType t i']

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
            ns <- replicateM (arityS sc) fresh
            pure $ foldr (\n -> Node t . Lam n) (Node t . Cons i' $ fmap Var ns) ns
        Nothing -> pure . Node t . Val $ Var i'

elabPrim :: SourcePos -> Primop -> Elaborator (Core SourcePos)
elabPrim t p = do
    ns <- replicateM (arityOp p) fresh
    pure $ foldr (\n -> Node t . Lam n) (Node t . Prim p $ fmap Var ns) ns

elabFun :: Syn.FunDef -> Elaborator (Core SourcePos)
elabFun (Syn.FunDef t mt _ args exp) =
    let lamexp = Syn.Lambda (Syn.Lam args exp)
        synexp = case mt of
            Just t  -> Syn.Annot (Syn.Expl (Node t lamexp) t)
            Nothing -> lamexp
    in elabExprNode t synexp

elabVal :: Syn.ValDef -> Elaborator (Control SourcePos)
elabVal (Syn.ValDef t (Just ty) _ exp) = elabExprNode t (Syn.Annot (Syn.Expl exp ty))
elabVal (Syn.ValDef t _ _ exp) = elabExpr exp

elabLam :: SourcePos -> Syn.Lam -> Elaborator (Core SourcePos)
elabLam t (Syn.Lam args exp) = do
    argm <- freshen args
    exp' <- withTerms argm (elabExpr exp)
    pure . Node t $ foldr (\(_,LocalIdentifier n) -> Node t . Lam n) exp' argm

elabMatch :: SourcePos -> Syn.Match -> Elaborator (Core SourcePos)
elabMatch t (Syn.Match e ps) = do
    n <- fresh
    e' <- elabExpr e
    exprCalls <- mapM (\(p,e) -> do
        fvexp <- freshen . Set.List $ pvars p
        e' <- withTerms fvexp (elabExpr e')
        argns <- case fvexp of
            (_:_)   -> pure $ fmap (\(_,LocalIdentifier n) -> n) fvexp
            []      -> fmap (:[]) fresh
        let lamexp = Node t $ foldr (\n -> Node t . Lam n) e' argns
        n <- fresh
        pure ((p,(n,getTag e,argns)),lamexp)) ps
    m <- matchComp t n (fmap fst exprCalls)
    pure . Node t . Fix (fmap (\((_,(n,_,_)),e) -> (n,e)) exprCalls) . Node t $ Let n e'

elabLet :: SourcePos -> Syn.Let -> Elaborator (Core SourcePos)
elabLet t (Syn.Let vs e) = do
    nvs <- mapM (\d@(Syn.ValDef _ _ n _) -> do
        e <- elabVal d
        f <- fresh
        pure ((LocalIdentifier n,LocalIdentifier f),e))
    e' <- withTerms (fmap fst nvs) (elabExpr e)
    pure $ foldr (\((_,LocalIdentifier n),e) -> Node t . Let n e) e' nvs

elabFix :: SourcePos -> Syn.Fix -> Elaborator (Core SourcePos)
elabFix t (Syn.Fix fs e) = do
    nfs <- mapM (\d@(Syn.FunDef _ _ n _ _) -> do
        e <- elabFun d
        f <- fresh
        pure ((LocalIdentifier n,LocalIdentifier f),e)) fs
    e' <- withTerms (fmap fst nfs) (elabExpr e)
    pure . Node t $ Fix (fmap (\((_,n),e) -> (n,e)) nfs) e'

elabTypeNode :: TypeNode -> Elaborator TypeNode
elabTypeNode (NamedType i) = do
    i' <- lookupType i
    pure (NamedType i')
elabTypeNode x = pure x

elabType :: Type -> Elaborator Scheme
elabType = fmap (generalize mempty . untag) (traverse elabTypeNode)

elabAnnot :: SourcePos -> Syn.Annotation Syn.Expr -> Elaborator (Core SourcePos)
elabAnnot t (Syn.Expl e ty) = do
    sc <- elabType ty
    e' <- elabExpr e
    pure . Node t (Annot e' sc)

elabExprNode :: SourcePos -> Syn.ExprNode -> Elaborator (Core SourcePos)
elabExprNode t (Syn.Var i) = elabIdent t i
elabExprNode t (Syn.Annot a) = elabAnnot t a
elabExprNode t (Syn.LetIn l) = elabLet t l
elabExprNode t (Syn.FixIn f) = elabFix t f
elabExprNode t (Syn.Lambda l) = elabLam t l
elabExprNode t (Syn.Literal l) = pure . Node t . Val $ Lit l
elabExprNode t (Syn.Switch m) = elabMatch t m
elabExprNode t (Syn.Primop p) = elabPrim t p

elabExpr :: Syn.Expr -> Elaborator (Core SourcePos)
elabExpr (App t a b) = do
    a' <- elabExpr a
    b' <- elabExpr b
    pure $ App t a' b'
elabExpr (Node t a) = elabExprNode t a

collectTypes :: Module -> [Syn.TopLevel] -> [(Identifier,Identifier)]
collectTypes m (Syn.Data (Syn.Ind n _ _):xs) =
    (LocalIdentifier n,ExternalIdentifier m n):collectTypes xs
collectTypes m (_:xs) = collectTypes m xs
collectTypes _ [] = []

collectTerms :: Module -> [Syn.TopLevel] -> [(Identifier,Identifier)]
collectTerms m (Syn.Func (Syn.FunDef _ _ n _ _):xs) =
    (LocalIdentifier n,ExternalIdentifier m n):collectTerms xs
collectTerms m (Syn.Data (Syn.Ind _ _ ns):xs) =
    fmap (\(Expl a _) -> (LocalIdentifier a,ExternalIdentifier m a)) ns ++ collectTerms xs
collectTerms _ [] = []

-- collect GADTs for environment purposes (and kindchecking, later?)
genGADTs :: [Syn.TopLevel] -> Elaborator [GADT]
genGADTs (Syn.Data (Syn.Ind n _ ns):xs) = do
    ns' <- mapM (\(Expl n t) -> fmap ((,) n) (elabType t)) ns
    xs' <- genGADTs xs
    pure (GADT n ns':xs')
genGADTs (_:xs) = genGADTs xs
genGADTs [] = pure []

genFns :: [Syn.TopLevel] -> Elaborator [(Name,Core SourcePos)]
genFns (Syn.Func d@(Syn.FunDef _ _ n _ _):xs) = do
    m <- mod
    let n' = ExternalIdentifier m n
    e' <- elabFun d
    xs' <- genFns xs
    pure ((n',e'):xs')

elabTL :: [Syn.TopLevel] -> Elaborator (Core SourcePos, [GADT])
elabTL xs = do
    m <- mod
    let t = initialPos (intercalate "." m)
    withTerms (collectTerms m xs) $ withTypes (collectTypes m xs) $ do
        fns <- genFns xs
        gadts <- genGADTs xs
        let exp = Node t $ Fix (fmap (first LocalIdentifier) fns) (Node t . Val $ Lit Unit)
        pure (exp, gadts)