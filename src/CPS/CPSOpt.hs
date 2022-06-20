{- assorted optimisations to occur after closure conversion, but before spilling -}
module CPS.CPSOpt (cpsOpt,cexpSize) where

import Types.CPS
import Types.Ident
import Types.Prim
import Control.Arrow
import Types.Pretty
import Types.Build
import Control.Monad.State

cpsOpt :: Int -> OptFlags -> CExp -> (CExp,Int)
cpsOpt s f e = flip runState s $ do
    e <- if cps_inline_sized f then inlineSized (cps_inline_size f) e else pure e
    if cps_inline_singles f then pure (inlineSingles e) else pure e

type Namer = State Int

-- inlining
usesOf :: Identifier -> CExp -> Int
usesOf i (App (Label j) _) | i == j = 1
usesOf i (Fix fns e) =
    sum (fmap (\(Fun _ _ _ e) -> usesOf i e) fns) + usesOf i e
usesOf i (Record _ _ e) = usesOf i e
usesOf i (Select _ _ _ e) = usesOf i e
usesOf i (Switch _ es) = sum (fmap (usesOf i) es)
usesOf i (Primop _ _ _ es) = sum (fmap (usesOf i) es)
usesOf _ _ = 0

substVarVal :: Identifier -> Value -> Value -> Value
substVarVal i v (Var j) | i == j = v
substVarVal i v (Label j) | i == j = v
substVarVal _ _ x = x

substVar :: Identifier -> Value -> CExp -> CExp
substVar i v (App f vs) = App (substVarVal i v f) (fmap (substVarVal i v) vs)
substVar i v (Fix fs e) =
    Fix
        (fmap (\(Fun d i as e)->Fun d i as (substVar i v e)) fs)
        (substVar i v e)
substVar i v (Record vs n e) =
    Record (fmap (first (substVarVal i v)) vs) n (substVar i v e)
substVar i v (Select ix va n e) =
    Select ix (substVarVal i v va) n (substVar i v e)
substVar i v (Switch x es) = Switch (substVarVal i v x) (fmap (substVar i v) es)
substVar i v (Primop p vs n es) =
    Primop p (fmap (substVarVal i v) vs) n (fmap (substVar i v) es)
substVar _ _ x = x

-- for inlining singly-called functions, no renaming is required
inlineDontRename :: CFun -> CExp -> CExp
inlineDontRename (Fun _ i as e) (App (Label j) xs) | i == j =
    foldr (uncurry substVar) e (zip as xs)
inlineDontRename f (Fix fs e) =
    Fix (fmap (\(Fun d i as e) -> Fun d i as (inlineDontRename f e)) fs)
        (inlineDontRename f e)
inlineDontRename f (Record vs i e) = Record vs i (inlineDontRename f e)
inlineDontRename f (Select i v n e) = Select i v n (inlineDontRename f e)
inlineDontRename f (Switch v es) = Switch v (fmap (inlineDontRename f) es)
inlineDontRename f (Primop p vs i es) = Primop p vs i (fmap (inlineDontRename f) es)
inlineDontRename _ x = x

cantRemove :: CFunData -> Bool
cantRemove d = issplit d || isexport d

canRemove :: CFunData -> Bool
canRemove = not . cantRemove

dropDeadFns :: CExp -> CExp
dropDeadFns (Fix fs e) =
    let kept = filter (\(Fun d i _ s) -> cantRemove d || usesOf i (Fix fs e) - usesOf i s /= 0) fs
    in if length kept == length fs then
        Fix fs e
    else
        dropDeadFns (Fix kept e)
dropDeadFns x = x

inlineSingles :: CExp -> CExp
inlineSingles e@(Fix fs _) =
    let toInline = filter (\(Fun d i _ s) -> canRemove d && usesOf i s == 0 && usesOf i e == 1) fs
    in if null toInline then
        e
    else
        inlineSingles (dropDeadFns (inlineDontRename (head toInline) e))
inlineSingles x = x

cexpSize :: CExp -> Int
cexpSize (Fix fs e) = 1 + sum (fmap (\(Fun _ _ _ e)->cexpSize e) fs) + cexpSize e
cexpSize (Record _ _ e) = 1 + cexpSize e
cexpSize (Select _ _ _ e) = 1 + cexpSize e
cexpSize (Switch _ es) = 1 + sum (fmap cexpSize es)
cexpSize (Primop _ _ _ es) = 1 + sum (fmap cexpSize es)
cexpSize _ = 0

fork :: Identifier -> Namer Identifier
fork x = case discardPath x of
    Gen i _ -> go i
    User i -> go i
    Symb i -> go i
    where
        go :: String -> Namer Identifier
        go i = do
            n <- get
            put (n+1)
            pure (LocalIdentifier (Gen i n))

renameBound :: CExp -> Namer CExp
renameBound (Fix fs e) = liftM2 Fix (renameFuns fs) (renameBound e)
renameBound (Record vs n e) = do
    n' <- fork n
    e' <- renameBound (substVar n (Var n') e)
    pure (Record vs n' e')
renameBound (Select i v n e) = do
    n' <- fork n
    e' <- renameBound (substVar n (Var n') e)
    pure (Select i v n' e')
renameBound (Switch v es) = Switch v <$> mapM renameBound es
renameBound (Primop p vs n es) = do
    n' <- fork n
    e' <- mapM (renameBound . substVar n (Var n')) es
    pure (Primop p vs n' e')
renameBound x = pure x

renameFuns :: [CFun] -> Namer [CFun]
renameFuns fs = do
    fs <- mapM (\f@(Fun _ n _ _)-> (,) f <$> fork n) fs
    let frs = fmap (\(Fun _ i _ _,i')->(i,i')) fs
    let fs' = fmap (\(Fun d _ as e,n')->Fun d n' as (foldr (\(i,i')->substVar i (Label i')) e frs)) fs
    forM fs' (\(Fun d i as e) -> do
        as <- forM as (\i -> (,) i . Var <$> fork i)
        let e' = foldr (uncurry substVar) e as
        pure (Fun d i (fmap (\(_,Var i)->i) as) e'))

inlineRenaming :: CFun -> CExp -> Namer CExp
inlineRenaming (Fun _ i as e) (App (Label j) vs) | i == j = renameBound (foldr (uncurry substVar) e (zip as vs))
inlineRenaming f (Fix fs e) =
    liftM2 Fix (mapM (\(Fun d i a e) -> Fun d i a <$> inlineRenaming f e) fs) (inlineRenaming f e)
inlineRenaming f (Record vs n e) = Record vs n <$> inlineRenaming f e
inlineRenaming f (Select i v n e) = Select i v n <$> inlineRenaming f e
inlineRenaming f (Switch v es) = Switch v <$> mapM (inlineRenaming f) es
inlineRenaming f (Primop p vs n es) = Primop p vs n <$> mapM (inlineRenaming f) es
inlineRenaming _ x = pure x

canInline :: Int -> CExp -> CFun -> Bool
canInline maxSize e (Fun d i _ e') =
    canRemove d && usesOf i e' == 0
    && cexpSize e' * (usesOf i e + 1) <= maxSize

selectInlineable :: Int -> CExp -> [CFun] -> (Maybe CFun,[CFun])
selectInlineable maxSize e (f:xs) | canInline maxSize e f =
    (Just f, xs)
selectInlineable maxSize e (f:xs) = let (s,us) = selectInlineable maxSize e xs in (s,f:us)
selectInlineable _ _ [] = (Nothing,[])

inlineSized :: Int -> CExp -> Namer CExp
inlineSized maxSize (Fix fns e) =
    let (toInline,fs) = selectInlineable maxSize e fns
    in case toInline of
        Just f -> inlineSized maxSize =<< inlineRenaming f (Fix fs e)
        Nothing -> pure (Fix fns e)
inlineSized _ x = pure x