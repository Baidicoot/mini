module Frontend.IRify where

-- conversion between AST and uniquely-named IR

import Types.Syntax hiding(ExprNode(..), Let(..), Lam(..))
import qualified Types.Syntax as Syntax
import Types.Ident
import Types.IR
import Types.Graph

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad

import qualified Data.Map.Strict as Map

type IRState = [Name]
type IREnv = Map.Map Identifier Identifier

data IRError
    = Unbound Identifier
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
withEnv ts = local (Map.union (Map.fromList ts))

lookupEnv :: Identifier -> IRifier Identifier
lookupEnv id = do
    env <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> throwError (Unbound id)

irifyDefn :: Definition -> IRifier IR
irifyDefn (Defn mt _ args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irify expr)
    let ir = foldr (\(_, LocalIdentifier a) l -> Node () $ Lam a l) def argns
    case mt of
        Just t -> pure . Node () $ Annot ir t
        Nothing -> pure ir

irifyLam :: Syntax.Lam -> IRifier IR
irifyLam (Syntax.Lam args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irify expr)
    pure (foldr (\(_, LocalIdentifier a) l -> Node () $ Lam a l) def argns)

irifyNode :: Syntax.ExprNode -> IRifier IR
irifyNode (Syntax.Var id) = do
    id <- lookupEnv id
    pure (Node () $ Var id)
irifyNode (Syntax.Annot (Expl a t)) = do
    a <- irify a
    pure (Node () $ Annot a t)
irifyNode (Syntax.LetIn (Syntax.Let defs expr)) = do
    ns <- mapM (\(Defn _ n _ _) -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) defs
    newdefs <- withEnv ns $ mapM (\(d, (_, LocalIdentifier f)) -> do
        ir <- irifyDefn d
        pure (f, ir)) (defs `zip` ns)
    expr <- withEnv ns (irify expr)
    pure (Node () $ Let newdefs expr)
irifyNode (Syntax.Lambda l) = irifyLam l

irify :: Expr -> IRifier IR
irify = fmap join . traverse irifyNode