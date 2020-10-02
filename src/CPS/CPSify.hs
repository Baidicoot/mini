module CPS.CPSify where

import Types.CPS
import Types.Ident
import Types.Prim
import Types.Env
import Types.Graph (NoTag)
import qualified Types.IR as IR
import qualified Types.Graph as Graph

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import Control.Monad
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (partition)

import qualified Data.Map as Map

type CPSState = ([Name], [Name])
type CPSEnv = Map.Map Identifier (Int, Int)

fresh :: CPSifier Name
fresh = do
    (names, ks) <- get
    let (n:ns) = names
    put (ns, ks)
    pure n

index :: Identifier -> CPSifier Int
index id = do
    env <- ask
    case Map.lookup id env of
        Just (x, _) -> pure x
        Nothing -> error "catastrophic failure, leaking launch codes..."

cont :: CPSifier Name
cont = do
    (ns, konts) <- get
    let (k:ks) = konts
    put (ns, ks)
    pure k

contNames :: [Name]
contNames = fmap (("k"++) . show) [0..]

cpsNames :: [Name]
cpsNames = fmap (("c"++) . show) [0..]

type CPSifier = StateT CPSState (Reader CPSEnv)

runCPSify :: [Name] -> CPSEnv -> CPSifier a -> (a, CPSState)
runCPSify ns e a = runReader (runStateT a (ns, contNames)) e

cpsify :: Dataspace -> IR.PolyIR typ NoTag -> CExp
cpsify (Dataspace env) exp = fst $ runCPSify cpsNames (fmap (\(a, b, _) -> (a, b)) env) (convert exp (\z -> pure Halt))

convertNode :: IR.PolyIRNode typ NoTag -> (Value -> CPSifier CExp) -> CPSifier CExp
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
convertNode (IR.Let n def e) c = do
    j <- fmap LocalIdentifier cont
    ce <- convert e c
    cd <- convert def (\z -> pure $ App (Var j) [z])
    pure $ Fix [Fun j [n] ce] cd
convertNode (IR.Cons id args) c = do
    i <- index id
    let cont = fmap (flip (,) (OffPath 0) . Var . LocalIdentifier) args
    x <- fresh
    convC <- c (Var $ LocalIdentifier x)
    pure $ Record ((Unboxed (Int i), OffPath 0):cont) x convC
convertNode (IR.Select i exp) c = do
    w <- fresh
    cw <- c (Var $ LocalIdentifier w)
    convert exp (\v -> pure $ Select i v w cw)
convertNode (IR.Match n exps) c = do
    j <- fmap LocalIdentifier cont
    x <- fresh
    cx <- c (Var $ LocalIdentifier x)
    let jfn = Fun j [x] cx
    (dflt, ncases, brmap) <- fmap (flip sortBranches exps) ask
    (brmap, conts) <- fmap (fmap fst &&& fmap (snd . snd) . Map.toList) $ mapM (\(ns, ir) -> do
        k <- fmap LocalIdentifier cont
        x <- fresh
        e <- convert ir (\z -> pure $ App (Var j) [z])
        let def = foldr (\(n, i) e -> Select i (Var $ LocalIdentifier x) n e) e (zip ns [1..])
        pure (k, Fun k [x] def)) brmap
    dflt <- mapM (flip convert (\z -> pure $ App (Var j) [z])) dflt
    d <- fmap LocalIdentifier cont
    discard <- fresh
    let dfn = Fun d [discard] (fromMaybe MatchError dflt)
    let cexps = fmap (\i -> App (Var $ Map.findWithDefault d i brmap) [Var $ LocalIdentifier n]) [0..ncases-1]
    tag <- fresh
    pure $ Select 0 (Var $ LocalIdentifier n) tag (Fix (jfn:dfn:conts) $ Switch (Var $ LocalIdentifier tag) cexps)

sortBranches :: Map.Map Identifier (Int, Int) -> [(IR.IRPattern, IR.PolyIR typ NoTag)] -> (Maybe (IR.PolyIR typ NoTag), Int, Map.Map Int ([Name], IR.PolyIR typ NoTag))
sortBranches idm br =
    let (wild, normal) = first (fmap snd . listToMaybe) $ partition ((==IR.IRWild) . fst) br
        (brmap, casenums) = unzip $ fmap (\(IR.IRCons id ns, exp) ->
            let (index, ncases) = idm Map.! id in ((index, (ns, exp)), ncases)) normal
        in (wild, fromMaybe 0 (listToMaybe casenums), Map.fromList brmap)
{-
Not sure about matches;
match x
  Just y -> a y
  Nothing -> b NoTag

converts to (?):
match x
  Just y -> a y k
  Nothing -> b NoTag k
where k is the current continuation?
-}

convert :: IR.PolyIR typ NoTag -> (Value -> CPSifier CExp) -> CPSifier CExp
convert (Graph.App _ f e) c = do
    r <- cont
    x <- fresh
    bigF <- convert f (\f -> convert e (\e -> pure $ App f [e, Var (LocalIdentifier r)]))
    convC <- c (Var (LocalIdentifier x))
    pure $ Fix [Fun (LocalIdentifier r) [x] convC] bigF
convert (Graph.Node _ n) c = convertNode n c