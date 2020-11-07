module Elaborate.Elaborate (elaborate, ElabWarning(..), ElabError(..), MatchWarning(..)) where

import Types.Pattern
import Types.Core
import Types.Env
import Types.Ident
import Types.Graph
import Types.Type
import Types.Prim

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

type Call tag = (Name, [Name], tag)

type ElabEnv = (Module, Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier Scheme)
type ElabState = Int
type Elaborator = ErrorsT [ElabError] (RWS ElabEnv [ElabWarning] ElabState)

elaborate :: Int -> Module -> Env -> [Syn.TopLevel] -> Either ([ElabError], [ElabWarning]) (Core SourcePos, [GADT], Int, [ElabWarning])
elaborate i m e tl =
    let env = (m, termRenames e, typeRenames e, fmap (\(a,b,c)->b) (consInfo e))
        (res, s, w) = runElab (elabTL tl) env i
    in case res of
        Right (c, g) -> Right (c, g, s, w)
        Left e -> Left (e, w)

runElab :: Elaborator a -> ElabEnv -> ElabState -> (Either [ElabError] a, ElabState, [ElabWarning])
runElab m r s = runRWS (runErrorsT m) r s

matchComp :: SourcePos -> Name -> [(Pattern SourcePos, Call SourcePos)] -> Elaborator (Core SourcePos)
matchComp t n ps = do
    i <- get
    let (c,n',w) = matchcomp i n t ps
    case w of
        (_:_)   -> tell [MatchWarning t w]
        []      -> pure ()
    put n'
    pure c

modul :: Elaborator Module
modul = fmap (\(m,_,_,_) -> m) ask

fresh :: Elaborator Name
fresh = do
    n <- get
    put (n+1)
    pure ('v':show n)

freshen :: [Name] -> Elaborator [(Identifier, Identifier)]
freshen = mapM (\n -> fmap ((,) (LocalIdentifier n) . LocalIdentifier) fresh)

withTerms :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTerms xs = local (\(m,a,b,c) -> (m,Map.fromList xs `mappend` a,b,c))

withTypes :: [(Identifier, Identifier)] -> Elaborator a -> Elaborator a
withTypes xs = local (\(m,a,b,c) -> (m,a,Map.fromList xs `mappend` b,c))

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
        pure ((p,(n,argns,getTag e)),lamexp)) ps
    m <- matchComp t n (fmap fst exprCalls)
    pure . Node t . Fix (fmap (\((_,(n,_,_)),e) -> (LocalIdentifier n,e)) exprCalls) . Node t $ Let n e' m

elabLet :: SourcePos -> Syn.Let -> Elaborator (Core SourcePos)
elabLet t (Syn.Let vs e) = do
    nvs <- mapM (\d@(Syn.ValDef _ _ n _) -> do
        e <- elabVal d
        f <- fresh
        pure ((LocalIdentifier n,LocalIdentifier f),e)) vs
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

elabType :: SourceType -> Elaborator Type
elabType (Node p (NamedType i)) = do
    i' <- lookupType p i
    pure . Node NoTag $ NamedType i'
elabType (App p a b) = do
    a' <- elabType a
    b' <- elabType b
    pure (App NoTag a' b')
elabType x = pure (untag x)

elabAnnot :: SourcePos -> Syn.Annotation Syn.Expr -> Elaborator (Core SourcePos)
elabAnnot t (Syn.Expl e ty) = do
    sc <- elabType ty
    e' <- elabExpr e
    pure . Node t $ Annot e' sc

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
    (LocalIdentifier n,ExternalIdentifier m n):collectTypes m xs
collectTypes m (_:xs) = collectTypes m xs
collectTypes _ [] = []

collectTerms :: Module -> [Syn.TopLevel] -> [(Identifier,Identifier)]
collectTerms m (Syn.Func (Syn.FunDef _ _ n _ _):xs) =
    (LocalIdentifier n,ExternalIdentifier m n):collectTerms m xs
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

genFns :: [Syn.TopLevel] -> Elaborator [(Identifier,Core SourcePos)]
genFns (Syn.Func d@(Syn.FunDef _ _ n _ _):xs) = do
    m <- modul
    let n' = ExternalIdentifier m n
    e' <- elabFun d
    xs' <- genFns xs
    pure ((n',e'):xs')
genFns (x:xs) = genFns xs
genFns [] = pure []

elabTL :: [Syn.TopLevel] -> Elaborator (Core SourcePos, [GADT])
elabTL xs = do
    m <- modul
    let t = initialPos (intercalate "." m)
    withTerms (collectTerms m xs) $ withTypes (collectTypes m xs) $ do
        fns <- genFns xs
        gadts <- genGADTs xs
        let exp = Node t $ Fix fns (Node t . Val $ Lit Unit)
        pure (exp, gadts)