module Frontend.Spill where

import Types.CPS
import Types.Ident

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Frontend.Spill - conversion of the CPS to limit free variables to be under N.
-- comprised of 2 individual passes
-- the first pass creates closures for functions with arguments > N.
-- the second pass does the actual work eliminating free variables > N in expressions.

type Spill = ReaderT Int (State [Name])

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
    if n <= length args then
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

maybeToSet = Set.fromList . maybeToList
filterFree (free, _) (rn, rc, scope) = (rn, Set.filter (`Set.member` free) rc, Map.filterWithKey ((`Set.member` free) . const) scope)

unique :: SpillCtx -> Set.Set Name
unique (_, rc, scope) = Map.keysSet $ Map.filterWithKey ((`Map.notMember` rc) . const) scope

duplicated :: SpillCtx -> Set.Set Name
duplicated (_, rc, scope) = Map.keysSet $ Map.filterWithKey ((`Map.member` scope) . const) rc

-- drop variables based off their distance from use
dropN :: Int -> SpillCtx -> VarInfo -> SpillCtx
dropN n ctx@(rn, rc, scope) (fv, depth) =
    let duped = duplicated ctx
    let fduped = Set.filter (`Set.member` fv) duped
    let ordered = sortBy (\n -> case Map.lookup n depth of
        Just x -> -x
        Nothing -> 0) . Set.toList $ fduped
    let dropping = take n ordered in
        (rn, Map.filterWithKey ((`Set.member` dropping) . const) rc, scope)

keepN :: Int -> Set.Set Name -> Set.Set Name -> VarInfo -> (Set.Set Name, Set.Set Name)
keepN n uniq duped (fv, depth) =
    let ordered = order uniq
    in (take n ordered, drop n ordered)
    where
        order = reverse . sortBy (\n -> case Map.lookup n depth of
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
    let ndup = n - Set.length new
    let (keeping, dropping) = keepN ndup (unique ctx) (duplicated ctx) info
    let needsSpill = Set.length dropping > 0
    -- split into functions etc here
    argSelects <- fmap unzip . foldM (\arg (fn, map) -> case Map.lookup arg rc of
        Just i -> do
            f <- fresh
            let binding = Select i (Var $ LocalIdentifier arg) f
            pure (binding . fn, (arg, f))
        Nothing -> pure (fn, (arg, arg))) $ Set.toList args
    (rn', rc', rfn) <- if needsSpill then do
            let vars = dropping `Set.union` Map.keysSet rc
            (accesses, locations) = case rn of
                Just rn -> foldr (\(v, ni) (acs, locs) -> case (Map.lookup v rc, Map.lookup v scope) of
                    (Just oi, _) ->
                        let acs' = (Var $ LocalIdentifier rn, SelPath oi (OffPath 0)):acs
                        in (acs', (v, ni):locs)
                    (_, Just ov) ->
                        let acs' = (Var $ LocalIdentifier ov, OffPath 0)
                        in (acs', (v, ni):locs)
                    _ -> error "unreachable") (zip (Set.toList vars) [0..])
                Nothing -> foldr (\(v, ni) (acs, locs) -> case (Map.lookup v scope) of
                    Just ov ->
                        let acs' = (Var $ LocalIdentifier ov, OffPath 0)
                        in (acs', (v, ni):locs)
                    _ -> error "unreachable") (zip (Set.toList vars) [0..])
            rn' <- fresh
            let binding = Record accesses rn'
            pure (Just rn', Map.fromList locations, binding)
        else
            pure (rn, rc, id)