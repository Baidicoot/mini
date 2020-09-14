{-# LANGUAGE LambdaCase #-}
module Frontend.ClosureConv where

-- currently this algorithm is horrendusly innefficent
-- it makes a polynomial number of passes of the graph
-- this is mostly for the simplicity of the algorithm
-- a similar effect can probably be achived in 1-2 passes

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
-- restrict search to ONLY LocalIdentifiers?

-- cache of function -> (fvs, known calls) for known functions
-- iteration used to handle cycles
type FunctionMeta = Map.Map Identifier (Set.Set Name, Set.Set Name)

setFM :: (Ord b) => (a -> Maybe b) -> Set.Set a -> Set.Set b
setFM f a = foldr (\a acc -> case f a of
    Just x -> mappend (Set.singleton x) acc
    Nothing -> acc) mempty a

valueToName :: Value -> Maybe Name
valueToName = (\case
    Var (LocalIdentifier id) -> Just id
    Label (LocalIdentifier id) -> Just id
    _ -> Nothing)

identToName :: Identifier -> Maybe Name
identToName = (\case
    LocalIdentifier id -> Just id
    _ -> Nothing)

escaping :: CExp -> Set.Set Name
escaping (App _ vs) = Set.fromList . catMaybes . fmap valueToName $ vs
escaping (Fix fns exp) = mappend (escaping exp) $ mconcat . fmap (\(Fun _ _ exp) -> escaping exp) $ fns
escaping (Record vs _ exp) = mappend (escaping exp) $ Set.fromList . catMaybes . fmap (valueToName . fst) $ vs
escaping (Select _ _ _ exp) = escaping exp
escaping (Switch _ exps) = mconcat (fmap escaping exps)
escaping (Primop _ vs _ exps) = mappend (mconcat $ fmap escaping exps) $ Set.fromList . catMaybes . fmap valueToName $ vs
escaping _ = mempty

functions :: CExp -> Set.Set Name
functions (Fix fns exp) = mappend (mconcat . fmap (\case
    Fun (LocalIdentifier n) _ exp -> Set.singleton n `mappend` functions exp
    Fun _ _ exp -> functions exp) $ fns) (functions exp)
functions (Record _ _ exp) = functions exp
functions (Select _ _ _ exp) = functions exp
functions (Switch _ exps) = mconcat . fmap functions $ exps
functions (Primop _ _ _ exps) = mconcat . fmap functions $ exps
functions _ = mempty

-- only handles CExp that it is expected to meet
fv :: CExp -> Set.Set Name
fv (App n vs) = Set.fromList . catMaybes . fmap valueToName $ (n:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . catMaybes . fmap (\(Fun id _ _) -> identToName id) $ fns) $ Set.union (fv exp) . mconcat $ fmap (\(Fun _ args exp) -> fv exp `Set.difference` Set.fromList args) fns
fv (Record vs n exp) = Set.delete n $ (fv exp) `Set.union` (Set.fromList . catMaybes . fmap (valueToName . fst) $ vs)
fv (Select _ v n exp) = Set.delete n $ Set.fromList (maybeToList (valueToName v)) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` (Set.fromList . maybeToList . valueToName $ v)
fv (Primop _ args n exps) = Set.delete n $ mconcat (fmap fv exps) `Set.union` (Set.fromList . catMaybes . fmap valueToName $ args)
fv _ = mempty

calls :: CExp -> Set.Set Name
calls (App (Var (LocalIdentifier v)) _) = Set.singleton v
calls (App (Label (LocalIdentifier v)) _) = Set.singleton v
calls (Fix fns exp) = foldr mappend (calls exp) (fmap (\(Fun _ _ exp) -> calls exp) fns)
calls (Record _ _ exp) = calls exp
calls (Select _ _ _ exp) = calls exp
calls (Switch _ exps) = mconcat (fmap calls exps)
calls (Primop _ _ _ exps) = mconcat (fmap calls exps)
calls _ = mempty

functionMeta :: Set.Set Name -> CExp -> FunctionMeta
functionMeta known (Fix fns exp) =
    foldr mappend (functionMeta known exp) (fmap (\(Fun id args exp) ->
        let free = Set.difference (fv exp) (known `Set.union` Set.fromList args)
            knownCalls = Set.intersection (calls exp) known
        in Map.singleton id (free, knownCalls) `Map.union` functionMeta known exp) fns)
functionMeta known (Record _ _ exp) = functionMeta known exp
functionMeta known (Select _ _ _ exp) = functionMeta known exp
functionMeta known (Switch _ exps) = mconcat (fmap (functionMeta known) exps)
functionMeta known (Primop _ _ _ exps) = mconcat (fmap (functionMeta known) exps)
functionMeta _ _ = mempty

reduceFunction :: (Set.Set Name, Set.Set Name) -> FunctionMeta -> (Set.Set Name, Set.Set Name)
reduceFunction (ns, kc) map = foldr mappend (ns, mempty) (fmap (map Map.!) (fmap LocalIdentifier . Set.toList $ kc))

reduceOnce :: FunctionMeta -> FunctionMeta
reduceOnce map = fmap (flip reduceFunction map) map

reduce :: FunctionMeta -> FunctionMeta
reduce map = let map' = reduceOnce map in if map' == map then map' else reduce map'

type ClosureEnv = (Set.Set Name, Set.Set Name, Map.Map Name [Name], Set.Set Name)
-- ClosureEnv - escaping functions, fix-bound functions, freevars for each function, called variables

type ClosureConv = ReaderT ClosureEnv (State ([Name], Map.Map Name Name))

fresh :: ClosureConv Name
fresh = do
    (names, a) <- get
    let (n:ns) = names
    put (ns, a)
    pure n

split :: Name -> ClosureConv Name
split n = do
    (a, map) <- get
    case Map.lookup n map of
        Just b -> pure b
        Nothing -> do
            f <- fresh
            let m' = Map.insert n f map
            put (a, m')
            pure f

escapes :: Name -> ClosureConv Bool
escapes id = do
    (env, _, _, _) <- ask
    pure $ id `Set.member` env

isKnown :: Name -> ClosureConv Bool
isKnown id = do
    (_, fns, _, _) <- ask
    pure . not $ id `Set.member` fns

getFv :: Name -> ClosureConv [Name]
getFv id = do
    (_, _, meta, _) <- ask
    pure $ meta Map.! id

isCalled :: Name -> ClosureConv Bool
isCalled id = do
    (_, _, _, called) <- ask
    pure $ id `Set.member` called

{-
RULES FOR CLOSURE CONVERSION:

upon encountering an escaping function f, create a new function f'
this new function f' destructs a record and calls f directly with arguments

when an escaping f is encountered, create a record of the free variables of f
and bind to a closure with f' as the #0 of that record

when a function accepts a closure argument `g` that is later called, it is renamed to `g_`,
and the function `g` is extracted from #0 of `g_`.

-}

convertArg :: Value -> ClosureConv (Value, CExp -> CExp)
convertArg (Var (LocalIdentifier f)) = do
    nameKnown <- isKnown f
    if nameKnown then do
        f' <- split f
        v <- fresh
        free <- getFv f
        let binding = Record (fmap (\(var, index) -> (Var $ LocalIdentifier var, OffPath index)) (zip (f':free) [0..])) v
        pure (Var $ LocalIdentifier v, binding)
    else
        pure (Var $ LocalIdentifier f, id)
convertArg x = pure (x, id)

convert :: CExp -> ClosureConv CExp
convert (App (Var (LocalIdentifier name)) vs) = do
    nameKnown <- isKnown name
    if nameKnown then do
        free <- getFv name
        pure . App (Var $ LocalIdentifier name) $ vs++(fmap (Var . LocalIdentifier) free)
    else do
        pure . App (Var $ LocalIdentifier name) $ vs++[Var . LocalIdentifier $ name++"_"]
convert (Fix fns exp) = do

{-
type ClosureConv = ReaderT (Set.Set Identifier, Set.Set Name, Map.Map Identifier [Name]) (State [Name])

-- argument capping is done in a separate pass (that is, putting extra arguments into closures)

{-
The following holds for local functions:
If one of a given function's arguments (call it `f`) is called, it is immediately, at the start of the function;
  - renamed `f_`
  - split into a function `f`

Upon an `(App v vs)`;
    - if v is not a known function, an extra argument `v_` is passed to it
    - if v is a known function that escapes, an extra argument `v_` is passed to it
    - forall `a` in vs, where `a` is a known function;
      - replace `a` with `a_`
      - bind `a_` to a closure of `a`

The following holds for global functions:
Leave as normal. Global functions cannot capture.
-}

extraArgs :: CFun -> ClosureConv CFun
extraArgs (Fun id args exp) = do
    args <- fmap ((args ++) . Set.toList) (getFv id)
    (exp', args') <- fmap (second reverse) $ foldM (\(exp, args) arg -> do
        getsCalled <- isCalled (LocalIdentifier arg)
        if getsCalled then
            let arg_ = arg ++ "_"
                bindarg = Select 0 (Var $ LocalIdentifier arg_) arg
            in
                (bindarg exp, arg_:args)
        else
            pure $ (exp, arg:args)) (exp, []) args
    pure $ Fun id args' exp'

makeClosure :: CFun -> ClosureConv CFun
makeClosure fn@(Fun id@(LocalIdentifier name) args exp) = do
    fnEsc <- escapes id
    if fnEsc then do
        let name_ = name ++ "_"
            bindargs = foldr (\(arg, index) -> Select index (Var $ LocalIdentifier name_) arg) (zip args [1..])
        in
            pure $ Fun id [name_] (bindargs exp)
    else
        pure fn
makeClosure x = pure x

convertFun :: CFun -> ClosureConv CFun
convertFun (Fun id args exp) = do
    exp' <- convert exp
    fn <- extraArgs (Fun id args exp')
    makeClosure fn

makeArgClosure :: Name -> ClosureConv (Name, CExp -> CExp)
makeArgClosure name = do
    let v = LocalIdentifier name
    known <- isKnown v
    reqcls <- escapes v
    if known && reqcls then do
        freevars <- getFv v
        let name_ = name ++ "_"
        let binding = Record (fmap (\(arg, index) -> (Value $ LocalIdentifier arg, OffPath index)) (zip (name:freevars) [0..])) name_
        pure (name_, binding)
    else
        pure (name, id)

convert :: CExp -> ClosureConv CExp
convert (App (Var v@(LocalIdentifier name)) vs) = do
    known <- isKnown v
    reqcls <- escapes v
    if known && reqcls then do
        freevars <- getFv v
        let name_ = name ++ "_"
        let freevarBinding = Record (fmap (\(arg, index) -> (Value $ LocalIdentifier arg, OffPath index)) (zip freevars [1..])) name_
        (args, fns) <- mapM (\case
            Var (LocalIdentifier n) -> fmap (first (Var . LocalIdentifier)) $ makeArgClosure n
            x -> pure (x, id)) vs
         

{-
convert (App id args) c = do
    isKnown <- isKnownCall id
    needsClosure <- escapes id
    fvs <- getFv id
    if isKnown && needsClosure then do
        n <- fresh
        cv <- c (Var $ LocalIdentifier n)
        pure $ Record (fmap (\(v, p) -> (Var $ LocalIdentifier v, OffPath p)) (zip fv [1..])) n cv
    else if isKnown then
        pure $ 
-}
-}