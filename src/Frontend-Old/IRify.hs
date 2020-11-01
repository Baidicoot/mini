{-# LANGUAGE LambdaCase #-}

module Frontend.IRify where

-- conversion between AST and uniquely-named IR + match compilation

import Types.Syntax hiding(ExprNode(..), Let(..), Lam(..), Match(..), Data(..), Fix(..))
import qualified Types.Syntax as Syntax
import Types.Ident
import Types.IR
import Types.Graph
import Types.Pattern
import Types.Type
import Types.Env
import Types.Prim
import Frontend.GenEnv

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import Control.Monad
import Data.List (intercalate, partition)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.Parsec.Pos

tmpPos :: SourcePos
tmpPos = initialPos "tmp"

type IRState = Int
type IREnv = (Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier (Int, Int, Int))

data IRError
    = Unbound Identifier
    | Rebound Name
    | VarLeft Name
    | WildLeft
    | OverlappingPatterns
    deriving(Show)

type IRifier = StateT IRState (ReaderT IREnv (Except IRError))

irify :: Dataspace -> Namespace -> [TopLevel] -> Either IRError (([Ind], IR), IRState)
irify (Dataspace ds) ns tl =
    let (nsm, csm) = genImportMap (Include ns)
    in runIRifier 0 (nsm, csm, ds) (irifyTL tl)

runIRifier :: IRState -> IREnv -> IRifier a -> Either IRError (a, IRState)
runIRifier s e = runExcept . flip runReaderT e . flip runStateT s

fresh :: IRifier Name
fresh = do
    n <- get
    put (n+1)
    pure ('v':show n)

withEnv :: [(Identifier, Identifier)] -> IRifier a -> IRifier a
withEnv ts p = do
    (expr, typ, s) <- ask
    local (const (Map.union (Map.fromList ts) expr, typ, s)) p

lookupEnv :: Identifier -> IRifier Identifier
lookupEnv id = do
    (env, _, _) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> throwError (Unbound id)

lookupType :: Identifier -> IRifier Identifier
lookupType id = do
    (_, env, _) <- ask
    case Map.lookup id env of
        Just x -> pure x
        Nothing -> throwError (Unbound id)

lookupCons :: Identifier -> IRifier (Maybe IR)
lookupCons id = do
    (_, _, s) <- ask
    case Map.lookup id s of
        Just (_, _, 0) -> pure . Just . Node NoTag $ Cons id []
        _ -> pure Nothing

irifyFun :: FunDef -> IRifier IR
irifyFun (FunDef mt _ args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irifyExpr expr)
    let ir = foldr (\(_, LocalIdentifier a) l -> Node NoTag $ Lam a l) def argns
    case mt of
        Just t -> do
            t <- irifyType (untag t)
            pure . Node NoTag $ Annot ir t
        Nothing -> pure ir

irifyVal :: ValDef -> IRifier IR
irifyVal (ValDef mt _ expr) = do
    ir <- irifyExpr expr
    case mt of
        Just t -> do
            t <- irifyType (untag t)
            pure . Node NoTag $ Annot ir t
        Nothing -> pure ir

irifyLam :: Syntax.Lam -> IRifier IR
irifyLam (Syntax.Lam args expr) = do
    argns <- mapM (\n -> do
        f <- fresh
        pure (LocalIdentifier n, LocalIdentifier f)) args
    def <- withEnv argns (irifyExpr expr)
    pure (foldr (\(_, LocalIdentifier a) l -> Node NoTag $ Lam a l) def argns)

{-
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
-}

irifyMap :: Ord k => (Map.Map k a -> n -> IRifier (n, Map.Map k a)) -> AppGraph n -> IRifier (AppGraph n, Map.Map k a)
irifyMap irifyNode = internal Map.empty
    where
        internal env (App NoTag a b) = do
            (a, env) <- internal env a
            (b, env) <- internal env b
            pure (App NoTag a b, env)
        internal env (Node NoTag a) = do
            (a, env) <- irifyNode env a
            pure (Node NoTag a, env)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = foldr (\x -> maybe (second (x:)) (\y -> first (y:)) (f x)) ([],[])

{-
MatchComp strategy - temporary, stand-in:
- select all cases matching on the top-level the first case
- build up dag for that case
- work left-to-right for variable subexpressions

for example:
case x of
    Pair a B -> a
    Pair A b -> b

gets transformed to:
case a of
    Pair b c -> case b of
        d -> case c of
            B -> d
            e -> case b of
                A -> e
                _ -> raise Match

which is optimized to:
case a of
    Pair b c -> case c of
        B -> b
        _ -> case b of
            A -> c
            _ -> raise Match

-}

-- Branch - name of expression function + argument order
type Branch = (Name, [Name])
-- MatchResult - holds bound patterns (new -> old), argument renames (old -> new)
type MatchResult = (Map.Map Name Pattern, Map.Map Name Name)
-- PartialMatch - holds remaining pattern, branch arm
type PartialMatch = (MatchResult, Branch)

union :: MatchResult -> MatchResult -> MatchResult
union (a, b) (c, d) = (a `Map.union` c, b `Map.union` d)

match :: Pattern -> IRPattern -> Maybe MatchResult
match _ IRWild = pure (mempty, mempty)
match (Node NoTag (PatternVar _)) _ = Nothing
match (Node NoTag (PatternCons id)) (IRCons id2 [])
    | id == id2 = pure (mempty, mempty)
match (App NoTag a b@(Node NoTag (PatternVar n))) (IRCons id args@(_:_)) =
    let x = last args
        xs = init args in do
            (ps, ns) <- match a (IRCons id xs)
            pure (ps, Map.insert n x ns)
match (App NoTag a b) (IRCons id args@(_:_)) =
    let x = last args
        xs = init args in do
            (ps, ns) <- match a (IRCons id xs)
            pure (Map.insert x b ps, ns)
match _ _ = Nothing

{-
partitionByVar :: Name -> [PartialMatch] -> ([(Pattern, PartialMatch)], [PartialMatch])
partitionByVar v parts = let (inc, notinc) = partition (Map.member v . fst . fst) parts in
    (   fmap (\((pats, bound), info) -> (pats Map.! v, ((Map.delete v pats, bound), info))) inc
    ,   notinc)
-}

-- incorrect completeness function - need to instead check all patterns are variables or wildcards
complete :: PartialMatch -> Bool
complete ((_, d), (_, ns)) = foldr (\n a -> a && (Map.member n d)) True ns

patComplete :: Pattern -> Bool
patComplete (App _ _ _) = False
patComplete (Node _ (PatternCons _)) = False
patComplete _ = True

nextVar :: PartialMatch -> Maybe Name
nextVar = listToMaybe . fmap fst . filter (not . patComplete . snd) . Map.toList . fst . fst

nextVarLs :: [PartialMatch] -> Maybe Name
nextVarLs ls = do
    a <- listToMaybe ls
    nextVar a

matchByVar :: Name -> [PartialMatch] -> [(Pattern, PartialMatch)]
matchByVar n = fmap (\((pats, bound), info) -> (Map.findWithDefault (Node NoTag PatternWildcard) n pats, ((Map.delete n pats, bound), info)))

patScope :: IRPattern -> [Name]
patScope (IRCons _ ns) = ns
patScope _ = []

groupByCons :: [(Pattern, PartialMatch)] -> IRifier [(IRPattern, [PartialMatch])]
groupByCons matches@((p, _):_) = do
    l <- leftmost p
    let (new, rem) = partitionByMatch l matches
    rem' <- groupByCons rem
    pure ((l, new):rem')
    where
        leftmost :: Pattern -> IRifier IRPattern
        leftmost (Node NoTag (PatternVar n)) = pure IRWild
        leftmost (Node NoTag PatternWildcard) = pure IRWild
        leftmost x = fmap (uncurry IRCons) $ internal x
            where
                internal (Node NoTag (PatternCons id)) = pure (id, [])
                internal (App NoTag a _) = do
                    n <- fresh
                    (id, ns) <- internal a
                    pure (id, n:ns)
                internal (Node NoTag (PatternVar n)) = throwError (VarLeft n)
                internal (Node NoTag PatternWildcard) = throwError WildLeft

        partitionByMatch :: IRPattern -> [(Pattern, PartialMatch)] -> ([PartialMatch], [(Pattern, PartialMatch)])
        partitionByMatch p = partitionMaybe (\(t, (res, b)) -> do
            res' <- match t p
            pure (res `union` res', b))
groupByCons [] = pure []

data MatchTree
    = Switch Name [(IRPattern, MatchTree)]
    | ExprBranch Name [Name]
    | MatchExcept
    deriving(Eq, Show)

optimizeMatchTree :: MatchTree -> MatchTree
optimizeMatchTree (Switch _ [(IRWild, m)]) = optimizeMatchTree m
optimizeMatchTree (Switch n xs) = Switch n (fmap (second optimizeMatchTree) xs)
optimizeMatchTree x = x

buildMatchTree :: Maybe Name -> [PartialMatch] -> IRifier MatchTree
buildMatchTree Nothing [m@((_, r), (f, ns))]
    | complete m = pure $ ExprBranch f (fmap (r Map.!) ns)
buildMatchTree Nothing [] = pure MatchExcept
buildMatchTree Nothing _ = throwError OverlappingPatterns
buildMatchTree (Just n) matches = do
    let b = matchByVar n matches
    br <- groupByCons b
    ts <- mapM (\(p, ms) -> do
        mt <- buildMatchTree (nextVarLs ms) ms
        pure (p, mt)) br
    pure (Switch n ts)

buildIRFromMatchTree :: MatchTree -> IR
buildIRFromMatchTree (ExprBranch n []) = 
    App NoTag (Node NoTag $ Var $ LocalIdentifier n) (Node NoTag $ Unboxed Unit)
buildIRFromMatchTree (ExprBranch n args) =
    foldr (\n a -> App NoTag a (Node NoTag (Var $ LocalIdentifier n))) (Node NoTag (Var $ LocalIdentifier n)) args
buildIRFromMatchTree (Switch n cases) =
    Node NoTag $ Match n (fmap (second buildIRFromMatchTree) cases)
buildIRFromMatchTree MatchExcept = undefined

renamePattern :: Pattern -> IRifier (Pattern, Map.Map Identifier Name)
renamePattern (App NoTag a b) = do
    (a', env0) <- renamePattern a
    (b', env1) <- renamePattern b
    pure (App NoTag a' b', env0 `mappend` env1)
renamePattern (Node NoTag (PatternVar n)) = do
    b <- fresh
    pure (Node NoTag (PatternVar b), Map.singleton (LocalIdentifier n) b)
renamePattern (Node NoTag (PatternCons id)) = do
    id' <- lookupEnv id
    pure (Node NoTag (PatternCons id'), mempty)
renamePattern x = pure (x, mempty)

makeCase :: Name -> (Pattern, Expr) -> IRifier ((Identifier, IR), PartialMatch)
makeCase n (p', exp) = do
    (p, env') <- renamePattern p'
    let env = Map.toList env'
    f <- fresh
    e <- withEnv (fmap (second LocalIdentifier) env) $ irifyExpr exp
    ir <- case env of
        [] -> do
            f <- fresh
            pure $ Node NoTag $ Lam f e
        _ -> pure $ foldr (\(_, n) a -> Node NoTag (Lam n a)) e env
    let branch = (f, fmap snd env)
    let matchres = (Map.singleton n p, Map.empty)
    pure ((LocalIdentifier f, ir), (matchres, branch))

makeCases :: Name -> [(Pattern, Expr)] -> IRifier ([(Identifier, IR)], [PartialMatch])
makeCases b cases = do
    cs <- mapM (makeCase b) cases
    let (ces, parts) = unzip cs
    pure (ces, parts)

irifyMatch :: Syntax.Match -> IRifier IR
irifyMatch (Syntax.Match exp cases) = do
    e <- irifyExpr exp
    b <- fresh
    (defs, parts) <- makeCases b (fmap (first untag) cases)
    mt <- buildMatchTree (Just b) parts
    let ir = buildIRFromMatchTree mt
    pure (Node NoTag $ Let b e (Node NoTag $ Fix defs ir))

irifyLet :: [ValDef] -> Syntax.Expr -> IRifier IR
irifyLet (d@(ValDef _ n _):xs) expr = do
    ir <- irifyVal d
    f <- fresh
    xsexp <- withEnv [(LocalIdentifier n, LocalIdentifier f)] (irifyLet xs expr)
    pure (Node NoTag $ Let f ir xsexp)
irifyLet [] expr = do
    ir <- irifyExpr expr
    pure ir

irifyFix :: Bool -> [FunDef] -> Syntax.Expr -> IRifier IR
irifyFix tl defs expr = do
    ns <- mapM (\(FunDef _ n _ _) -> do
        f <- if tl then lookupEnv (LocalIdentifier n) else fmap LocalIdentifier fresh
        pure (LocalIdentifier n, f)) defs
    newdefs <- withEnv ns $ mapM (\(d, (_, f)) -> do
        ir <- irifyFun d
        pure (f, ir)) (defs `zip` ns)
    expr <- withEnv ns (irifyExpr expr)
    pure (Node NoTag $ Fix newdefs expr)

irifyNode :: Syntax.ExprNode -> IRifier IR
irifyNode (Syntax.Var id) = do
    id <- lookupEnv id
    syn <- lookupCons id
    case syn of
        Just syn -> pure syn
        Nothing -> pure (Node NoTag $ Var id)
irifyNode (Syntax.Annot (Expl a t)) = do
    a <- irifyExpr a
    t <- irifyType (untag t)
    pure (Node NoTag $ Annot a t)
irifyNode (Syntax.LetIn (Syntax.Let defs expr)) = irifyLet defs expr
irifyNode (Syntax.Lambda l) = irifyLam l
irifyNode (Syntax.Switch m) = irifyMatch m
irifyNode (Syntax.Literal l) = pure . Node NoTag $ Unboxed l
irifyNode (Syntax.FixIn (Syntax.Fix defs expr)) = irifyFix False defs expr

irifyPoly :: (a -> IRifier (TaggedAppGraph t b)) -> TaggedAppGraph t a -> IRifier (TaggedAppGraph t b)
irifyPoly irfn = fmap joinApp . traverse irfn

irifyExpr :: Expr -> IRifier IR
irifyExpr = irifyPoly irifyNode . untag

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

irifyData :: Syntax.Data -> IRifier Ind
irifyData (Syntax.Ind n mk cs) = do
    cs <- mapM (\(Expl a t) -> do
        n <- lookupEnv (LocalIdentifier a)
        t <- irifyType (untag t)
        pure (n, t)) cs
    n <- lookupType (LocalIdentifier n)
    pure (Ind n (fmap untag mk) cs)

sortTopLevel :: [TopLevel] -> ([Syntax.Data], [FunDef])
sortTopLevel ((Data d):xs) = let (da, de) = sortTopLevel xs in (d:da, de)
sortTopLevel ((Func f):xs) = let (da, de) = sortTopLevel xs in (da, f:de)
sortTopLevel [] = ([], [])

irifyTLD :: [FunDef] -> IRifier IR
irifyTLD de = irifyFix True de (Node tmpPos (Syntax.Literal Unit))

nullary :: Type -> Bool
nullary = (==0) . arity

nullaryS :: Scheme -> Bool
nullaryS (Forall _ t) = nullary t

curriedCons :: [Ind] -> IRifier ([(Identifier, IR)])
curriedCons ((Ind _ _ is):ids) = do
    hcs <- curriedCons ids
    fhs <- mapM (\(i,Forall _ t) -> do
        --i' <- lookupEnv i
        vars <- replicateM (arity t) fresh
        pure $ (i, foldr (\v acc -> Node NoTag $ Lam v acc) (Node NoTag $ Cons i vars) vars)) . filter (not . nullaryS . snd) $ is
    pure (fhs++hcs)
curriedCons [] = pure []

-- todo: create env things
irifyTL :: [TopLevel] -> IRifier ([Ind], IR)
irifyTL ts = let (da, de) = sortTopLevel ts in do
    dat <- mapM irifyData da
    cons <- curriedCons dat
    ir <- irifyTLD de
    let (ds, exp) = (case ir of -- case in do is *weird*
            Node _ (Fix a b) -> (a, b)
            e -> ([], e))
    pure (dat, Node NoTag $ Fix (cons++ds) exp)