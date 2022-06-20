module CPS.CPSify where

import Types.CPS
import Types.Ident
import Types.Prim
import Types.Module
import Types.Type
import Types.Graph (NoTag)
import qualified Types.Graph as Graph
import qualified Types.Core as Core

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Data.Maybe (isJust,isNothing)

import qualified Data.Map as Map

type CPSState = (Int, [Identifier])
type CPSEnv = (Map.Map Identifier Int, Map.Map Identifier Int, [Identifier])

mkCPSEnv :: [Identifier] -> [GADT] -> CPSEnv
mkCPSEnv exports env =
    let cons = Map.fromList $ concatMap (\(GADT _ _ ls) -> zip (fmap fst ls) [0..]) env
        gadts = Map.fromList $ fmap (\(GADT i _ ls) -> (i,length ls)) env
    in (cons, gadts, exports)

fresh :: CPSifier Name
fresh = do
    (n,known) <- get
    put (n+1,known)
    pure (Gen "v" n)

func :: CPSifier Name
func = do
    (n,known) <- get
    put (n+1,LocalIdentifier (Gen "f" n):known)
    pure (Gen "f" n)

fixdefs :: [Identifier] -> CPSifier ()
fixdefs ids = modify (second (ids++))

cont :: CPSifier Name
cont = do
    (n,known) <- get
    put (n+1,LocalIdentifier (Gen "k" n):known)
    pure (Gen "k" n)

convVal :: Value -> CPSifier Value
convVal (Var i) = do
    (_,known) <- get
    pure (if i `elem` known then Label i else Var i)
convVal x = pure x

index :: Identifier -> CPSifier Int
index id = do
    (env,_,_) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> error $ "oh bees oh bees oh bees oh bees" ++ show env

cases :: Identifier -> CPSifier Int
cases id = do
    (_,env,_) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> error $ "catastrophic failure, leaking launch codes..."

exported :: Identifier -> CPSifier Bool
exported i = asks (\(_,_,b)->i `elem` b)

type CPSifier = StateT CPSState (Reader CPSEnv)

runCPSify :: [Identifier] -> Int -> CPSEnv -> CPSifier a -> (a, CPSState)
runCPSify k ns e a = runReader (runStateT a (ns,k)) e

cpsify :: [Identifier] -> ModuleServer -> [Identifier] -> Core.Core NoTag -> Int -> (CExp, CPSState)
cpsify k env expo exp i = runCPSify k i (mkCPSEnv expo (gadts env)) (convert exp (\z -> pure Halt))

convertNode :: Core.CoreNode NoTag -> (Value -> CPSifier CExp) -> CPSifier CExp
convertNode (Core.Error s) c = pure (Error s)
convertNode (Core.Val v) c = convVal v >>= c
convertNode (Core.Lam v e) c = do
    f <- func
    k <- cont
    bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier k)) [z])
    convC <- c (Label (LocalIdentifier f))
    pure $ Fix [Fun cfun (LocalIdentifier f) [v, LocalIdentifier k] bigF] convC
convertNode (Core.Fix defs e) c = do
    fixdefs (fmap fst defs) -- unique binding means that this can be a *permanent* change
    bigF <- convert e c
    bigG <- g defs
    pure $ Fix bigG bigF
    where
        g ((n, Graph.Node _ (Core.Lam v e)):defs) = do
            w <- cont
            bigF <- convert e (\z -> pure $ App (Var (LocalIdentifier w)) [z])
            dx <- g defs
            ex <- exported n
            pure $ (Fun cfun{name = Just n, isexport = ex} n [v, LocalIdentifier w] bigF):dx
        g [] = pure []
convertNode (Core.Let n def e) c = do
    j <- fmap LocalIdentifier cont
    ce <- convert e c
    cd <- convert def (\z -> pure $ App (Label j) [z])
    pure $ Fix [Fun (cfun {islet = True, name = Just n}) j [n] ce] cd
convertNode (Core.Cons id args) c = do
    i <- index id
    cont <- fmap (\x -> (x,NoPath)) <$> mapM convVal args
    x <- fresh
    convC <- c (Var $ LocalIdentifier x)
    pure $ Record ((Lit (Int i), NoPath):cont) (LocalIdentifier x) convC
convertNode (Core.Tuple []) c = c (Lit $ Int 0)
convertNode (Core.Tuple xs) c = do
    cont <- fmap (\x -> (x,NoPath)) <$> mapM convVal xs
    x <- fresh
    convC <- c (Var $ LocalIdentifier x)
    pure $ Record cont (LocalIdentifier x) convC
convertNode (Core.Select i v) c = do
    x <- fresh
    v' <- convVal v
    ce <- c (Var $ LocalIdentifier x)
    pure $ Select i v' (LocalIdentifier x) ce
convertNode (Core.Match (Just ty, p) n cs) c | isJust (constructor ty) = do
    let (Just i) = constructor ty
    t <- fresh
    ncases <- cases i
    let (dflt, cs') = selectDefault cs
    casemap <- mapM (\b@(Core.ConsPattern cons _,_,_) -> flip (,) b <$> index cons) cs'
    fallback <- case dflt of
        Just fallback -> convert fallback c
        Nothing -> pure $ Error ("pattern match fail at: " ++ show p)
    cs'' <- flip mapM [0..ncases-1] $ \i -> case lookup i casemap of
            Nothing -> pure fallback
            Just (Core.ConsPattern _ ns, _, cse) ->
                let bindings = foldr (\(i,v) f -> Select i (Var n) (LocalIdentifier v) . f) id (zip [1..] ns)
                in fmap bindings (convert cse c)
    pure . Select 0 (Var n) (LocalIdentifier t) $ Switch (Var $ LocalIdentifier t) cs''
convertNode (Core.Match (Just (Graph.Node Graph.NoTag (Builtin t)), p) n cs) c = do
    let eq = eqOp t
    let (dflt, cs') = selectDefault cs
    fallback <- case dflt of
        Just fallback -> convert fallback c
        Nothing -> pure $ Error ("pattern match fail at: " ++ show p)
    foldM (\b (Core.LiteralPattern l,_,exp) -> do
        br <- convert exp c
        x <- fresh
        pure (Primop eq [Var n,Lit l] (LocalIdentifier x) [br,b])) fallback cs'
convertNode (Core.Prim p vs) c | isNothing (branchesOp p) = do
    x <- fresh
    vs' <- mapM convVal vs
    convC <- c (Var $ LocalIdentifier x)
    pure $ Primop p vs' (LocalIdentifier x) [convC]
convertNode (Core.Prim op ls) c | switchOp op && Just (length ls) >= branchesOp op = do
    x <- fresh
    let (Just n) = branchesOp op
    let args = take (length ls - n) ls
    let cases = drop (length ls - n) ls
    args' <- mapM convVal args
    cases' <- mapM (c <=< convVal) cases
    pure $ Primop op args' (LocalIdentifier x) cases'
convertNode x _ = error (show x)

selectDefault :: [(Core.PatternBinding, NoTag, Core.Core NoTag)] -> (Maybe (Core.Core NoTag), [(Core.PatternBinding, NoTag, Core.Core NoTag)])
selectDefault (p@(Core.WildcardPattern, _, e):_) = (Just e, [])
selectDefault (x:xs) = second (x:) (selectDefault xs)
selectDefault _ = (Nothing, [])

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
    bigF <- convert f (\f -> convert e (\e -> (\f -> App f [e, Label (LocalIdentifier r)]) <$> convVal f))
    convC <- c (Var (LocalIdentifier x))
    pure $ Fix [Fun (cfun {iscont = True}) (LocalIdentifier r) [LocalIdentifier x] convC] bigF
convert (Graph.Node _ n) c = convertNode n c