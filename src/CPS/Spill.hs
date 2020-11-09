module CPS.Spill where

import Types.CPS
import Types.Ident
import Types.Prim

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Maybe
import Data.List (sortBy)

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Frontend.Spill - conversion of the CPS to limit free variables to be under N.
-- comprised of 2 individual passes
-- the first pass creates closures for functions with arguments > N.
-- the second pass does the actual work eliminating free variables > N in expressions.

type Spill = ReaderT Int (State [Name])

spillNames :: [Name]
spillNames = fmap (("s"++) . show) [0..]

runSpill :: Int -> (Spill a) -> a
runSpill i p = fst . flip runState spillNames $ runReaderT p i

spill :: Int -> CExp -> CExp
spill i exp = runSpill i (do
    exp' <- overflowArgs exp
    spillFix exp' (Nothing, mempty, mempty))

fresh :: Spill Name
fresh = do
    names <- get
    let (n:ns) = names
    put ns
    pure n

overflowArgsFn :: CFun -> Spill CFun
overflowArgsFn (Fun id args exp) = do
    exp' <- overflowArgs exp
    n <- ask
    if n >= length args then
        pure $ Fun id args exp'
    else do
        c <- fresh
        let args' = take (n-1) args ++ [c]
        let incls = drop (n-1) args
        let bound = foldr (\(arg, i) exp -> Select i (Var $ LocalIdentifier c) arg exp) exp' (zip incls [0..])
        pure $ Fun id args' bound 

overflowArgs :: CExp -> Spill CExp
overflowArgs (Fix defs exp) = do
    defs' <- mapM overflowArgsFn defs
    exp' <- overflowArgs exp
    pure $ Fix defs' exp'
overflowArgs (App f args) = do
    n <- ask
    if n <= length args then
        pure $  App f args
    else do
        c <- fresh
        let args' = take (n-1) args ++ [Var $ LocalIdentifier c]
        let incls = drop (n-1) args
        pure $ Record (fmap (second OffPath) (zip incls [0..])) c (App f args')
overflowArgs (Record a b exp) = do
    exp' <- overflowArgs exp
    pure $ Record a b exp'
overflowArgs (Switch v exps) = do
    exps' <- mapM overflowArgs exps
    pure $ Switch v exps'
overflowArgs (Primop a b c exps) = do
    exps' <- mapM overflowArgs exps
    pure $ Primop a b c exps'
overflowArgs x = pure x

type SpillCtx = (Maybe Name, Map.Map Name Int, Map.Map Name Name)
-- spill record name, vars only in spill, vars in regs -> fresh names

{-
Spilling algo:
if for any expression the variable bound by that expression,
in addition to the variables currently bound in the expression's
scope exceed the number of available registers on that machine
then a record is made of the variables currently in scope
or in the previous spilling record that are also in the free variables
of the continuation expressions
-}

sortWith :: Ord a => (b -> a) -> [b] -> [b]
sortWith f = sortBy (\a b -> f a `compare` f b)

maybeToSet (Just a) = Set.singleton a
maybeToSet _ = mempty

filterFree (free, _) (rn, rc, scope) = (rn, Set.filter (`Set.member` free) rc, Map.filterWithKey (\k _ -> k `Set.member` free) scope)

unique :: SpillCtx -> Set.Set Name
unique (_, rc, scope) = Map.keysSet $ Map.filterWithKey (\k _ -> k `Map.notMember` rc) scope

duplicated :: SpillCtx -> Set.Set Name
duplicated (_, rc, scope) = Map.keysSet $ Map.filterWithKey (\k _ -> k `Map.member` scope) rc

-- drop variables based off their distance from use
dropN :: Int -> SpillCtx -> VarInfo -> SpillCtx
dropN n ctx@(rn, rc, scope) (fv, depth) =
    let duped = duplicated ctx
        fduped = Set.filter (`Set.member` fv) duped
        ordered = sortWith (\n -> case Map.lookup n depth of
            Just x -> -x
            Nothing -> 0) . Set.toList $ fduped
        dropping = take n ordered in
        (rn, Map.filterWithKey (\k _ -> k `elem` dropping) rc, scope)

keepN :: Int -> Set.Set Name -> Set.Set Name -> VarInfo -> (Set.Set Name, Set.Set Name)
keepN n uniq duped (fv, depth) =
    let ordered = order uniq
    in (Set.fromList $ take n ordered, Set.fromList $ drop n ordered)
    where
        order = reverse . sortWith (\n -> case Map.lookup n depth of
            Just x -> -x
            Nothing -> 0) . Set.toList

-- selectVars takes a spilling context, var order, bound variables, required variables
-- it returns a selector function for those variables and a new spilling context

-- variable renaming is captured in the returned spilling context
-- the new context needs to have all the 'arg' variables in scope
selectVars :: SpillCtx -> VarInfo -> Set.Set Name -> Set.Set Name -> Spill (SpillCtx, CExp -> CExp)
selectVars ctx@(rn, rc, scope) info bound args = do
    n <- ask
    let new = bound `Set.union` args
    let ndup = n - Set.size new - 1
    let (keeping, dropping) = keepN ndup (unique ctx) (duplicated ctx) info
    (argSelects, argNames) <- foldM (\(fn, map) arg -> case Map.lookup arg rc of
        Just i -> do
            f <- fresh
            let binding = Select i (Var $ LocalIdentifier arg) f
            pure (binding . fn, (arg, f):map)
        Nothing -> pure (fn, (arg, arg):map)) (id, []) $ Set.toList args
    (rn', rc', rfn) <- do
            let vars = dropping `Set.union` Map.keysSet rc
            if Set.size vars > 0 then do
                let (accesses, locations) = case rn of
                        Just rn -> foldr (\(v, ni) (acs, locs) -> case (Map.lookup v rc, Map.lookup v scope) of
                            (Just oi, _) ->
                                let acs' = (Var $ LocalIdentifier rn, SelPath oi (OffPath 0)):acs
                                in (acs', (v, ni):locs)
                            (_, Just ov) ->
                                let acs' = acs ++ [(Var $ LocalIdentifier ov, OffPath 0)]
                                in (acs', (v, ni):locs)
                            _ -> error "unreachable") ([], []) (zip (Set.toList vars) [0..])
                        Nothing -> foldr (\(v, ni) (acs, locs) -> case (Map.lookup v scope) of
                            Just ov ->
                                let acs' = acs ++ [(Var $ LocalIdentifier ov, OffPath 0)]
                                in (acs', (v, ni):locs)
                            _ -> error "unreachable") ([], []) (zip (Set.toList vars) [0..])
                rn' <- fresh
                let binding = Record accesses rn'
                pure (Just rn', Map.fromList locations, binding)
            else
                pure (rn, rc, id)
    let scope' = Map.fromList argNames `Map.union` Map.filterWithKey (\k _ -> k `Set.member` keeping) scope `Map.union` (Map.fromList . Set.toList $ Set.map (\x -> (x, x)) bound)
    pure ((rn', rc', scope'), argSelects . rfn)

getBoundAndArgs :: CExp -> (Set.Set Name, Set.Set Name)
getBoundAndArgs exp@(App _ _) = (mempty, fv exp)
getBoundAndArgs (Record _ n _) = (Set.singleton n, mempty)
getBoundAndArgs (Select _ v n _) = (Set.singleton n, Set.fromList $ extractNames [v])
getBoundAndArgs (Switch v _) = (mempty, Set.fromList $ extractNames [v])
getBoundAndArgs (Primop _ vs n _) = (Set.singleton n, Set.fromList $ extractNames vs)
getBoundAndArgs _ = (mempty, mempty)

makeSpillStep :: CExp -> SpillCtx -> Spill (SpillCtx, CExp -> CExp)
makeSpillStep exp ctx = let (bound, args) = getBoundAndArgs exp in selectVars ctx (getVarInfo exp) bound args

lookupValCtx :: Value -> SpillCtx -> Value
lookupValCtx v@(Var (LocalIdentifier n)) (_, _, scope) = case Map.lookup n scope of
    Just x -> Var $ LocalIdentifier x
    Nothing -> v
lookupValCtx v _ = v

lookupValsCtx :: [Value] -> SpillCtx -> [Value]
lookupValsCtx vs ctx = fmap (flip lookupValCtx ctx) vs

lookupValsRecord :: [(Value, AccessPath)] -> SpillCtx -> [(Value, AccessPath)]
lookupValsRecord accs (Just rn, rc, scope) = fmap (\(v, p) ->
        case v of
            Var (LocalIdentifier n) ->
                case (Map.lookup n scope, Map.lookup n rc) of
                    (Just o, _) -> (Var $ LocalIdentifier o, p)
                    (_, Just i) -> (Var $ LocalIdentifier rn, SelPath i p)
                    _ -> (Var $ LocalIdentifier n, p)
            _ -> (v, p)) accs
lookupValsRecord accs ctx = fmap (first (flip lookupValCtx ctx)) accs

spillExp :: CExp -> SpillCtx -> Spill CExp
spillExp exp@(App v vs) ctx = do
    (ctx', sfn) <- makeSpillStep exp ctx
    pure . sfn $ App (lookupValCtx v ctx') (lookupValsCtx vs ctx')
spillExp exp@(Record vs n sexp) ctx = do
    (ctx', sfn) <- makeSpillStep exp ctx
    sexp' <- spillExp sexp ctx'
    pure . sfn $ Record (lookupValsRecord vs ctx') n sexp'
spillExp exp@(Select i v n sexp) ctx = do
    (ctx', sfn) <- makeSpillStep exp ctx
    sexp' <- spillExp sexp ctx'
    pure . sfn $ Select i (lookupValCtx v ctx') n sexp'
spillExp exp@(Switch v sexps) ctx = do
    (ctx', sfn) <- makeSpillStep exp ctx
    sexps' <- mapM (flip spillExp ctx') sexps
    pure . sfn $ Switch (lookupValCtx v ctx') sexps'
spillExp exp@(Primop op vs n sexps) ctx = do
    (ctx', sfn) <- makeSpillStep exp ctx
    sexps' <- mapM (flip spillExp ctx') sexps
    pure . sfn $ Primop op (lookupValsCtx vs ctx') n sexps'
spillExp exp _ = pure exp

spillFix (Fix defs exp) ctx = do
    exp' <- spillExp exp ctx
    defs' <- mapM (\(Fun id args exp) -> do
        let scope = Map.fromList (fmap (\a -> (a, a)) args)
        exp' <- spillExp exp (Nothing, mempty, scope)
        pure $ Fun id args exp') defs
    pure $ Fix defs' exp'