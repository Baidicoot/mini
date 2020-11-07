module CPS.CPSify where

import Types.CPS
import Types.Ident
import Types.Prim
import Types.Env
import Types.Graph (NoTag)
import qualified Types.Core as Core
import qualified Types.Graph as Graph

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import Control.Monad
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (partition)

import qualified Data.Map as Map

type CPSState = Int
type CPSEnv = Map.Map Identifier (Int, Int)

mkCPSEnv :: Env -> CPSEnv
mkCPSEnv = fmap (\(i,_,GADT _ ls) -> (i,length ls)) . consInfo

fresh :: CPSifier Name
fresh = do
    n <- get
    put (n+1)
    pure ('v':show n)

index :: Identifier -> CPSifier Int
index id = do
    env <- ask
    case Map.lookup id env of
        Just (x, _) -> pure x
        Nothing -> error "catastrophic failure, leaking launch codes..."

cont :: CPSifier Name
cont = do
    n <- get
    put (n+1)
    pure ('k':show n)

contNames :: [Name]
contNames = fmap (("k"++) . show) [0..]

cpsNames :: [Name]
cpsNames = fmap (("c"++) . show) [0..]

type CPSifier = StateT CPSState (Reader CPSEnv)

runCPSify :: Int -> CPSEnv -> CPSifier a -> (a, CPSState)
runCPSify ns e a = runReader (runStateT a ns) e

cpsify :: Env -> Core.Core NoTag -> Int -> (CExp, CPSState)
cpsify env exp i = runCPSify i (mkCPSEnv env) (convert exp (\z -> pure Halt))

convertNode :: Core.CoreNode NoTag -> (Value -> CPSifier CExp) -> CPSifier CExp
convertNode (Core.Val v) c = c v
convertNode (Core.Lam v e) c = do
    f <- fresh
    k <- cont
    bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier k)) [z])
    convC <- c (Var (LocalIdentifier f))
    pure $ Fix [Fun (LocalIdentifier f) [v, k] bigF] convC
convertNode (Core.Fix defs e) c = do
    bigF <- convert e c
    bigG <- g defs
    pure $ Fix bigG bigF
    where
        g ((n, Graph.Node _ (Core.Lam v e)):defs) = do
            w <- cont
            bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier w)) [z])
            dx <- g defs
            pure $ (Fun n [v, w] bigF):dx
        g [] = pure []
convertNode (Core.Let n def e) c = do
    j <- fmap LocalIdentifier cont
    ce <- convert e c
    cd <- convert def (\z -> pure $ App (Var j) [z])
    pure $ Fix [Fun j [n] ce] cd
convertNode (Core.Cons id args) c = do
    i <- index id
    let cont = fmap (flip (,) (OffPath 0)) args
    x <- fresh
    convC <- c (Var $ LocalIdentifier x)
    pure $ Record ((Lit (Int i), OffPath 0):cont) x convC
{-
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
-}
convert :: Core.Core NoTag -> (Value -> CPSifier CExp) -> CPSifier CExp
convert (Graph.App _ f e) c = do
    r <- cont
    x <- fresh
    bigF <- convert f (\f -> convert e (\e -> pure $ App f [e, Var (LocalIdentifier r)]))
    convC <- c (Var (LocalIdentifier x))
    pure $ Fix [Fun (LocalIdentifier r) [x] convC] bigF
convert (Graph.Node _ n) c = convertNode n c