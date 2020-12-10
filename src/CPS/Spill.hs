module CPS.Spill (spill) where

import qualified Data.Set as Set

import Types.Ident
import Types.Prim
import Types.CPS
import Types.Pretty

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader

type Spill = StateT Int (Reader Int)

runSpill :: Int -> Int -> Spill a -> (a, Int)
runSpill n s p = flip runReader n $ runStateT p s

knownFns :: CExp -> Set.Set Name
knownFns (Fix fs _) = Set.fromList . extractLocals $ fmap (\(Fun i _ _) -> i) fs

spill :: Int -> Int -> CExp -> (CExp, Int)
spill n s e = runSpill n s $ do
        e' <- overflowArgs e
        spillFix e'

fresh :: Spill Name
fresh = do
    n <- get
    put (n+1)
    pure ('s':show n)

maybeToSet :: Maybe a -> Set.Set a
maybeToSet (Just a) = Set.singleton a
maybeToSet Nothing = Set.empty

renameVal :: Identifier -> Identifier -> Value -> Value
renameVal v v' (Var n)
    | n == v = Var v'
renameVal _ _ x = x

rename :: Identifier -> Identifier -> CExp -> CExp
rename n n' (App v vs) = App (renameVal n n' v) (fmap (renameVal n n') vs)
rename n n' (Record ps r e) = Record (fmap (first $ renameVal n n') ps) r (rename n n' e)
rename n n' (Select i v s e) = Select i (renameVal n n' v) s (rename n n' e)
rename n n' (Switch v es) = Switch (renameVal n n' v) (fmap (rename n n') es)
rename n n' (Primop o vs p es) = Primop o (fmap (renameVal n n') vs) p (fmap (rename n n') es)
rename _ _ x = x

argsRoot :: CExp -> Set.Set Identifier
argsRoot (App v vs) = Set.fromList $ extractIdents (v:vs)
argsRoot (Record ps _ _) = Set.fromList $ extractIdents (fmap fst ps)
argsRoot (Select _ v _ _) = Set.fromList $ extractIdents [v]
argsRoot (Switch v _) = Set.fromList $ extractIdents [v]
argsRoot (Primop _ vs _ _) = Set.fromList $ extractIdents vs
argsRoot _ = mempty

boundRoot :: CExp -> Set.Set Identifier
boundRoot (Select _ _ n _) = Set.singleton n
boundRoot (Primop _ _ n _) = Set.singleton n
boundRoot (Record _ n _) = Set.singleton n
boundRoot _ = mempty

contRoot :: CExp -> [CExp]
contRoot (Select _ _ _ c) = [c]
contRoot (Record _ _ c) = [c]
contRoot (Switch _ cs) = cs
contRoot (Primop _ _ _ cs) = cs
contRoot _ = []

root :: CExp -> [CExp] -> CExp
root (Select a b c _) [d] = Select a b c d
root (Record a b _) [c] = Record a b c
root (Switch a _) cs = Switch a cs
root (Primop a b c _) cs = Primop a b c cs
root x _ = x

cull :: Set.Set Identifier -> Int -> Set.Set Identifier
cull s i
    | i <= 0 = mempty
    | Set.size s > i = cull (Set.deleteMax s) (i-1)
    | otherwise = s

spillExp :: Set.Set Identifier -> Set.Set Identifier -> Set.Set Identifier -> [Identifier] -> Maybe Identifier -> CExp -> Spill CExp
spillExp _ _ _ _ _ Halt = pure Halt
spillExp r u d sc sv e = do
    n <- ask
    let a = argsRoot e
    let w = boundRoot e
    let c = contRoot e
    let vbefore = fv e
    let vafter = mconcat (fmap fv c)
    let sbefore = maybeToSet sv
    let safterv = if null c then Nothing else sv
    let safterc = if null c then mempty else sc
    let ndup = n - Set.size sbefore - Set.size ((u `Set.intersection` vbefore) `Set.union` r)
    let d' = cull (d `Set.intersection` vbefore) ndup
    let argsSpill = Set.size (a `Set.union` (u `Set.intersection` vafter)) > n - length safterv
    let boundSpill = Set.size (w `Set.union` (u `Set.intersection` vafter)) > n - length safterv
    if argsSpill || boundSpill then do
        sv' <- fresh
        let d'' = (u `Set.union` d') `Set.intersection` vbefore
        let sc' = Set.toList vbefore
        let ps = fmap (\i -> (Var i,SelPath (indexOf i sc) NoPath)) sc'
        e' <- spillExp mempty mempty d'' sc' (Just $ LocalIdentifier sv') e
        pure $ Record ps (LocalIdentifier sv') e'
    else do
        let f = a `Set.difference` (u `Set.intersection` d')
        case (Set.size f, sv) of
            (1,Just sv) -> do
                let v = Set.findMin f
                let i = indexOf v sc
                v' <- fresh
                e' <- spillExp mempty (u `Set.intersection` vbefore) (Set.insert (LocalIdentifier v') d') safterc safterv (rename v (LocalIdentifier v') e)
                pure $ Select i (Var sv) (LocalIdentifier v') e'
            (_,Just sv) -> do
                let v = Set.findMin f
                let i = indexOf v sc
                v' <- fresh
                e' <- spillExp mempty (u `Set.intersection` vbefore) (Set.insert (LocalIdentifier v') d') sc (Just sv) (rename v (LocalIdentifier v') e)
                pure $ Select i (Var sv) (LocalIdentifier v') e'
            (_,_) -> do
                c' <- mapM (spillExp w (u `Set.intersection` vafter) d' safterc safterv) c
                pure $ root e c'
    where
        indexOf :: (Show a,Eq a) => a -> [a] -> Int
        indexOf a (a':as)
            | a == a' = 0
            | otherwise = indexOf a as + 1
        indexOf a [] = error ("could not find index of " ++ show a ++ " in " ++ pretty e(0::Int))

spillFix :: CExp -> Spill CExp
spillFix (Fix defs e) = do
    defs' <- mapM (\(Fun i args e) -> do
        e' <- spillExp mempty (Set.fromList args) mempty [] Nothing e
        pure $ Fun i args e') defs
    Fix defs' <$> spillExp mempty mempty mempty [] Nothing e

overflowArgsFn :: CFun -> Spill CFun
overflowArgsFn (Fun id args exp) = do
    exp' <- overflowArgs exp
    n <- ask
    if n >= length args then
        pure $ Fun id args exp'
    else do
        c <- fresh
        let args' = take (n-1) args ++ [LocalIdentifier c]
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
    if n >= length args then
        pure $ App f args
    else do
        c <- fresh
        let args' = take (n-1) args ++ [Var $ LocalIdentifier c]
        let incls = drop (n-1) args
        pure $ Record (fmap (second (`SelPath` NoPath)) (zip incls [0..])) (LocalIdentifier c) (App f args')
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