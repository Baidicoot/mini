module Frontend.IRify where

-- conversion between AST and uniquely-named IR

import Types.Syntax hiding(ExprNode(..), Let(..), Lam(..), Match(..))
import qualified Types.Syntax as Syntax
import Types.Ident
import Types.IR
import Types.Graph
import Types.Pattern
import Types.Type

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

type IRState = [Name]
type IREnv = (Map.Map Identifier Identifier, Map.Map Identifier Identifier)

data IRError
    = Unbound Identifier
    | Rebound Name
    deriving(Show)

type IRifier = StateT IRState (ReaderT IREnv (Except IRError))

runIRifier :: IRState -> IREnv -> IRifier a -> Either IRError (a, IRState)
runIRifier s e = runExcept . flip runReaderT e . flip runStateT s

evalIRifier :: IRState -> IREnv -> IRifier a -> Either IRError a
evalIRifier s e = runExcept . flip runReaderT e . flip evalStateT s

fresh :: IRifier Name
fresh = do
    (n:ns) <- get
    put ns
    pure n

withEnv :: [(Identifier, Identifier)] -> IRifier a -> IRifier a
withEnv ts p = do
    (expr, typ) <- ask
    local (const (Map.union (Map.fromList ts) expr, typ)) p

lookupEnv :: Identifier -> IRifier Identifier
lookupEnv id = do
    (env, _) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> throwError (Unbound id)

lookupType :: Identifier -> IRifier Identifier
lookupType id = do
    (_, env) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> throwError (Unbound id)

irifyDefn :: Definition -> IRifier IR
irifyDefn (Defn mt _ args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irifyExpr expr)
    let ir = foldr (\(_, LocalIdentifier a) l -> Node () $ Lam a l) def argns
    case mt of
        Just t -> do
            t <- irifyType t
            pure . Node () $ Annot ir t
        Nothing -> pure ir

irifyLam :: Syntax.Lam -> IRifier IR
irifyLam (Syntax.Lam args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irifyExpr expr)
    pure (foldr (\(_, LocalIdentifier a) l -> Node () $ Lam a l) def argns)

irifyPatNode :: Map.Map Identifier Identifier -> PatternNode -> IRifier (PatternNode, Map.Map Identifier Identifier)
irifyPatNode env (PatternVar n) =
    case Map.lookup (LocalIdentifier n) env of
        Just _ -> throwError (Rebound n)
        Nothing -> do
            f <- fresh
            pure (PatternVar f, env `Map.union` Map.singleton (LocalIdentifier n) (LocalIdentifier f))
irifyPatNode env (PatternCons n) = do
    n <- lookupEnv n
    pure (PatternCons n, env)
irifyPatNode env x = pure (x, env)

irifyPat :: Pattern -> IRifier (Pattern, Map.Map Identifier Identifier)
irifyPat = irifyMap irifyPatNode

irifyMap :: Ord k => (Map.Map k a -> n -> IRifier (n, Map.Map k a)) -> AppGraph n -> IRifier (AppGraph n, Map.Map k a)
irifyMap irifyNode = internal Map.empty
    where
        internal env (App () a b) = do
            (a, env) <- internal env a
            (b, env) <- internal env b
            pure (App () a b, env)
        internal env (Node () a) = do
            (a, env) <- irifyNode env a
            pure (Node () a, env)

irifyMatch :: Syntax.Match -> IRifier IR
irifyMatch (Syntax.Match expr cases) = do
    irexp <- irifyExpr expr
    ircases <- mapM (\(p, e) -> do
        (p, env) <- irifyPat p
        e <- withEnv (Map.toList env) (irifyExpr e)
        pure (p, e)) cases
    pure (Node () (Match irexp ircases))

irifyNode :: Syntax.ExprNode -> IRifier IR
irifyNode (Syntax.Var id) = do
    id <- lookupEnv id
    pure (Node () $ Var id)
irifyNode (Syntax.Annot (Expl a t)) = do
    a <- irifyExpr a
    t <- irifyType t
    pure (Node () $ Annot a t)
irifyNode (Syntax.LetIn (Syntax.Let defs expr)) = do
    ns <- mapM (\(Defn _ n _ _) -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) defs
    newdefs <- withEnv ns $ mapM (\(d, (_, LocalIdentifier f)) -> do
        ir <- irifyDefn d
        pure (f, ir)) (defs `zip` ns)
    expr <- withEnv ns (irifyExpr expr)
    pure (Node () $ Let newdefs expr)
irifyNode (Syntax.Lambda l) = irifyLam l
irifyNode (Syntax.Switch m) = irifyMatch m

irifyPoly :: (a -> IRifier (AppGraph b)) -> AppGraph a -> IRifier (AppGraph b)
irifyPoly irfn = fmap join . traverse irfn

irifyExpr :: Expr -> IRifier IR
irifyExpr = irifyPoly irifyNode

irifyTypeNode :: Map.Map Name Name -> TypeNode -> IRifier (TypeNode, Map.Map Name Name)
irifyTypeNode env (NamedType n) = do
    n <- lookupType n
    pure (NamedType n, env)
irifyTypeNode env (TypeVar n) =
    case Map.lookup n env of
        Just x -> pure (TypeVar x, env)
        Nothing -> do
            f <- fresh
            pure (TypeVar f, Map.insert n f env)
irifyTypeNode env x = pure (x, env)

irifyType :: Type -> IRifier Scheme
irifyType = fmap (generalize mempty) . fmap (\(a, _) -> a) . irifyMap irifyTypeNode

data IRTL
    = Inductive Identifier (Maybe Kind) [(Identifier, Scheme)]
    | Function Identifier IR

instance Show IRTL where
    show (Inductive n mk ts) =
        let header = "ind " ++ show n
            annot = case mk of
                Just x -> " :: " ++ show x ++ "\n"
                Nothing -> "\n"
            body = concatMap (\(id, typ) -> "    " ++ show id ++ " :: " ++ show typ ++ "\n") ts in
                header ++ annot ++ body
    show (Function id ir) = show id ++ " = " ++ show ir ++ "\n"
    showList = const . concatMap ((++"\n") . show)

irifyData :: Data -> IRifier IRTL
irifyData (Ind n mk cs) = do
    cs <- mapM (\(Expl a t) -> do
        n <- lookupEnv (LocalIdentifier a)
        t <- irifyType t
        pure (n, t)) cs
    n <- lookupType (LocalIdentifier n)
    pure (Inductive n mk cs)

irifyTopLevel :: TopLevel -> IRifier IRTL
irifyTopLevel (Func d@(Defn _ n _ _)) = do
    d <- irifyDefn d
    n <- lookupEnv (LocalIdentifier n)
    pure (Function n d)
irifyTopLevel (Data ind) = irifyData ind

irify :: [TopLevel] -> IRifier [IRTL]
irify = mapM irifyTopLevel