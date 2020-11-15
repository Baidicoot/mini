module CPS.Spill (spill) where

import qualified Data.Set as Set

import Types.Ident
import Types.Prim
import Types.CPS

import Control.Arrow
import Control.Monad.State
import Control.Monad.Reader

type Spill = StateT Int (Reader (Int,Set.Set Name))

runSpill :: (Int,Set.Set Name) -> Int -> Spill a -> (a, Int)
runSpill n s p = flip runReader n $ runStateT p s

knownFns :: CExp -> Set.Set Name
knownFns (Fix fs _) = Set.fromList . extractLocals $ fmap (\(Fun i _ _) -> i) fs

spill :: Int -> Int -> CExp -> (CExp, Int)
spill n s e =
    let env = (n,knownFns e)
    in runSpill env s $ do
        e' <- overflowArgs e
        spillFix e'

indexOf :: (Eq a) => a -> [a] -> Int
indexOf a (a':as)
    | a == a' = 0
    | otherwise = indexOf a as + 1

maybeToSet :: Maybe a -> Set.Set a
maybeToSet (Just a) = Set.singleton a
maybeToSet _ = Set.empty

fresh :: Spill Name
fresh = do
    n <- get
    put (n+1)
    pure ('s':show n)

renameVal :: Name -> Name -> Value -> Value
renameVal v v' (Var (LocalIdentifier n))
    | n == v = Var $ LocalIdentifier v'
renameVal _ _ x = x

rename :: Name -> Name -> CExp -> CExp
rename n n' (App v vs) = App (renameVal n n' v) (fmap (renameVal n n') vs)
rename n n' (Record ps r e) = Record (fmap (first $ renameVal n n') ps) r (rename n n' e)
rename n n' (Select i v s e) = Select i (renameVal n n' v) s (rename n n' e)
rename n n' (Switch v es) = Switch (renameVal n n' v) (fmap (rename n n') es)
rename n n' (Primop o vs p es) = Primop o (fmap (renameVal n n') vs) p (fmap (rename n n') es)
rename _ _ x = x

namesFromVals :: Set.Set Name -> [Value] -> Set.Set Name
namesFromVals s = (`Set.difference` s) . Set.fromList . extractNames

-- need to distinguish between labels and arguments
argsRoot :: CExp -> Set.Set Name -> Set.Set Name
argsRoot (App v vs) s = namesFromVals s (v:vs)
argsRoot (Record ps _ _) s = namesFromVals s (fmap fst ps)
argsRoot (Select _ v _ _) s = namesFromVals s [v]
argsRoot (Switch v _) s = namesFromVals s [v]
argsRoot (Primop _ vs _ _) s = namesFromVals s vs
argsRoot _ _ = mempty

boundRoot :: CExp -> Set.Set Name
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

cull :: Set.Set Name -> Int -> Set.Set Name
cull s i
    | i <= 0 = mempty
    | Set.size s > i = cull (Set.deleteMax s) (i-1)
    | otherwise = s

spillExp :: Set.Set Name -> Set.Set Name -> Set.Set Name -> [Name] -> Maybe Name -> CExp -> Spill CExp
spillExp r u d sc sv e = do
    (n,k) <- ask
    let a = argsRoot e k
    let w = boundRoot e
    let c = contRoot e
    let vbefore = fv e
    let vafter = mconcat (fmap fv c)
    let sbefore = maybeToSet sv
    let safterv = if null c then mempty else sv
    let safterc = if null c then mempty else sc
    let ndup = n - Set.size sbefore - Set.size ((u `Set.intersection` vbefore) `Set.union` r)
    let d' = cull (d `Set.intersection` vbefore) ndup
    let argsSpill = Set.size (a `Set.union` (u `Set.intersection` vafter)) > n - length safterv
    let boundSpill = Set.size (w `Set.union` (u `Set.intersection` vafter)) > n - length safterv
    if argsSpill || boundSpill then do
        sv' <- fresh
        let d'' = (u `Set.union` d') `Set.intersection` vbefore
        let sc' = Set.toList vbefore
        let ps = fmap (\n -> (Var $ LocalIdentifier n,OffPath $ indexOf n sc)) sc'
        e' <- spillExp mempty mempty d'' sc' (Just sv') e
        pure $ Record ps sv' e'
    else do
        let f = a `Set.difference` (u `Set.intersection` d')
        case (Set.size f, sv) of
            (1,Just sv) -> do
                let v = Set.findMin f
                let i = indexOf v sc
                v' <- fresh
                e' <- spillExp mempty (u `Set.intersection` vbefore) (Set.insert v' d') safterc safterv (rename v v' e)
                pure $ Select i (Var $ LocalIdentifier sv) v' e'
            (_,Just sv) -> do
                let v = Set.findMin f
                let i = indexOf v sc
                v' <- fresh
                e' <- spillExp mempty (u `Set.intersection` vbefore) (Set.insert v' d') sc (Just sv) (rename v v' e)
                pure $ Select i (Var $ LocalIdentifier sv) v' e'
            (_,_) -> do
                c' <- mapM (spillExp w (u `Set.intersection` vafter) d' safterc safterv) c
                pure $ root e c'

spillFix :: CExp -> Spill CExp
spillFix (Fix defs e)  = do
    defs' <- mapM (\(Fun i args e) -> do
        e' <- spillExp mempty (Set.fromList args) mempty [] Nothing e
        pure $ Fun i args e') defs
    Fix defs' <$> spillExp mempty mempty mempty [] Nothing e

overflowArgsFn :: CFun -> Spill CFun
overflowArgsFn (Fun id args exp) = do
    exp' <- overflowArgs exp
    (n,_) <- ask
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
    (n,_) <- ask
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