module Frontend.CPSify where

import Types.CPS
import Types.Ident
import qualified Types.IR as IR
import qualified Types.Graph as Graph

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import Control.Monad

type CPSState = ([Name], [Name])

fresh :: CPSifier Name
fresh = do
    (names, ks) <- get
    let (n:ns) = names
    put (ns, ks)
    pure n

cont :: CPSifier Name
cont = do
    (ns, konts) <- get
    let (k:ks) = konts
    put (ns, ks)
    pure k

contNames :: [Name]
contNames = fmap (('k':) . show) [0..]

type CPSifier = State CPSState

runCPSify :: [Name] -> CPSifier a -> (a, CPSState)
runCPSify ns a = runState a (ns, contNames)

convertNode :: IR.PolyIRNode typ () -> (Value -> CPSifier CExp) -> CPSifier CExp
convertNode (IR.Var id) c = c (Var id)
convertNode (IR.Unboxed u) c = c (Unboxed u)
convertNode (IR.Lam v e) c = do
    f <- fresh
    k <- cont
    bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier k)) [z])
    convC <- c (Var (LocalIdentifier f))
    pure $ Fix [Fun (LocalIdentifier f) [v, k] bigF] convC
convertNode (IR.Fix defs e) c = do
    bigF <- convert e c
    bigG <- g defs
    pure $ Fix bigG bigF
    where
        g ((n, Graph.Node _ (IR.Lam v e)):defs) = do
            w <- cont
            bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier w)) [z])
            dx <- g defs
            pure $ (Fun n [v, w] bigF):dx
        g [] = pure []
convertNode (IR.Let n def e) c = convert (Graph.App () (Graph.Node () $ IR.Lam n e) def) c

convert :: IR.PolyIR typ () -> (Value -> CPSifier CExp) -> CPSifier CExp
convert (Graph.App _ f e) c = do
    r <- cont
    x <- fresh
    bigF <- convert f (\f -> convert e (\e -> pure $ App f [e, Var (LocalIdentifier r)]))
    convC <- c (Var (LocalIdentifier x))
    pure $ Fix [Fun (LocalIdentifier r) [x] convC] bigF
convert (Graph.Node _ n) c = convertNode n c