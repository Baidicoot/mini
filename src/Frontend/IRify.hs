{-# LANGUAGE LambdaCase #-}

module Frontend.IRify where

-- conversion between AST and uniquely-named IR + match compilation

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
import Control.Arrow
import Control.Monad
import Data.List (intercalate, partition)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type IRState = [Name]
type IREnv = (Map.Map Identifier Identifier, Map.Map Identifier Identifier)

data IRError
    = Unbound Identifier
    | Rebound Name
    | VarLeft Name
    | WildLeft
    | OverlappingPatterns
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
        internal env (App () a b) = do
            (a, env) <- internal env a
            (b, env) <- internal env b
            pure (App () a b, env)
        internal env (Node () a) = do
            (a, env) <- irifyNode env a
            pure (Node () a, env)

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
match (Node () (PatternVar _)) _ = Nothing
match (Node () (PatternCons id)) (IRCons id2 [])
    | id == id2 = pure (mempty, mempty)
match (App () a b@(Node () (PatternVar n))) (IRCons id args@(_:_)) =
    let x = last args
        xs = init args in do
            (ps, ns) <- match a (IRCons id xs)
            pure (ps, Map.insert n x ns)
match (App () a b) (IRCons id args@(_:_)) =
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
matchByVar n = fmap (\((pats, bound), info) -> (Map.findWithDefault (Node () PatternWildcard) n pats, ((Map.delete n pats, bound), info)))

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
        leftmost (Node () (PatternVar n)) = pure IRWild
        leftmost (Node () PatternWildcard) = pure IRWild
        leftmost x = fmap (uncurry IRCons) $ internal x
            where
                internal (Node () (PatternCons id)) = pure (id, [])
                internal (App () a _) = do
                    n <- fresh
                    (id, ns) <- internal a
                    pure (id, n:ns)
                internal (Node () (PatternVar n)) = throwError (VarLeft n)
                internal (Node () PatternWildcard) = throwError WildLeft

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
buildIRFromMatchTree (ExprBranch n args) =
    foldr (\n a -> App () a (Node () (Var $ LocalIdentifier n))) (Node () (Var $ LocalIdentifier n)) args
buildIRFromMatchTree (Switch n cases) =
    Node () $ Match n (fmap (second buildIRFromMatchTree) cases)
buildIRFromMatchTree MatchExcept = undefined

renamePattern :: Pattern -> IRifier (Pattern, Map.Map Identifier Name)
renamePattern (App () a b) = do
    (a', env0) <- renamePattern a
    (b', env1) <- renamePattern b
    pure (App () a' b', env0 `mappend` env1)
renamePattern (Node () (PatternVar n)) = do
    b <- fresh
    pure (Node () (PatternVar b), Map.singleton (LocalIdentifier n) b)
renamePattern x = pure (x, mempty)

makeCase :: Name -> (Pattern, Expr) -> IRifier ((Name, IR), PartialMatch)
makeCase n (p', exp) = do
    (p, env') <- renamePattern p'
    let env = Map.toList env'
    f <- fresh
    e <- withEnv (fmap (second LocalIdentifier) env) $ irifyExpr exp
    let ir = foldr (\(_, n) a -> Node () (Lam n a)) e env
    let branch = (f, fmap snd env)
    let matchres = (Map.singleton n p, Map.empty)
    pure ((f, ir), (matchres, branch))

makeCases :: Name -> [(Pattern, Expr)] -> IRifier ([(Name, IR)], [PartialMatch])
makeCases b cases = do
    cs <- mapM (makeCase b) cases
    let (ces, parts) = unzip cs
    pure (ces, parts)

irifyMatch :: Syntax.Match -> IRifier IR
irifyMatch (Syntax.Match exp cases) = do
    e <- irifyExpr exp
    b <- fresh
    (defs, parts) <- makeCases b cases
    mt <- buildMatchTree (Just b) parts
    let ir = buildIRFromMatchTree mt
    pure (Node () $ Let ((b, e):defs) ir)

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