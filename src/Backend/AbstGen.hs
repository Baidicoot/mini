module Backend.AbstGen (generateAbstract) where

import Types.Abstract
import Types.Ident
import Types.Prim
import qualified Types.CPS as CPS
import Types.CPS (CExp, CFun, AccessPath)
import qualified Types.Injection as Inj

import Prelude hiding(drop)
import Control.Monad
import Control.Arrow
import Control.Monad.RWS hiding(fix)
import Data.List (find, uncons, nub, intercalate)

import CPS.Meta

import qualified Data.Map as Map
import qualified Data.Set as Set

type AbstState = (Inj.Injection Name GPR, Map.Map Name [GPR], Int)
-- current register operand state, register layouts, current name
type AbstEnv = (Set.Set Name, Set.Set Name, Int)
-- known non-escaping functions, known escaping functions, number of registers available

runAbstGen :: AbstGen a -> AbstEnv -> (a, [Operator])
runAbstGen a e = (\(a,_,c) -> (a,c)) (runRWS a e (mempty, mempty, 0))

generateAbstract :: CExp -> Int -> [Operator]
generateAbstract exp regs =
    let metadata = collect exp
        funcs = Set.fromList . extractLocals $ Map.keys metadata
        escFns = (mconcat . fmap (Set.fromList . extractLocals . Set.toList . escaping . snd) $ Map.toList metadata) `Set.intersection` funcs
        nonescFns = funcs `Set.difference` escFns
    in
        snd (runAbstGen (generate exp) (nonescFns, escFns, regs))

type AbstGen = RWS AbstEnv [Operator] AbstState

-- observations:
-- if a called function is known, it is non-escaping
-- if a called function is unknown, it is escaping
-- escaping functions use r0 for first argument, etc.

max :: AbstGen Int
max = fmap (\(_,_,c) -> c) ask

emit :: Operator -> AbstGen ()
emit op = tell [op]

unused :: AbstGen GPR
unused = do
    (env,_,_) <- get
    let (Just a) = find (not . flip Inj.memberInv env) [0..]
    pure a

cull :: Set.Set Name -> AbstGen ()
cull free = modify (\(a,b,c) -> (Inj.filterFst (`Set.member` free) a, b, c))

getReg :: Set.Set Name -> AbstGen GPR
getReg free = do
    cull free
    unused

caseLabel :: AbstGen Int
caseLabel = do
    (a,b,c) <- get
    put (a,b,c+1)
    pure c

assoc :: Name -> GPR -> AbstGen ()
assoc n r = modify (\(a,b,c) -> (Inj.insert n r a, b, c))

getAssoc :: GPR -> AbstGen (Maybe Name)
getAssoc r = do
    (env, _, _) <- get
    pure . fmap fst $ Inj.lookupInv r env

getPrimaryReg :: Name -> AbstGen (Maybe GPR)
getPrimaryReg v = do
    (env, _, _) <- get
    case Inj.lookup v env of
        ((_, x):_) -> pure $ Just x
        _ -> pure Nothing

desoc :: GPR -> AbstGen ()
desoc r = modify (\(a,b,c) -> (Inj.deleteSnd r a, b, c))

keep :: [GPR] -> AbstGen ()
keep rs = modify (\(a,b,c) -> (Inj.filterSnd (`elem` rs) a, b, c))

record :: [(Value, AccessPath)] -> Name -> CExp -> AbstGen ()
record ps n exp = do
    ps' <- mapM (\(a,b) -> fmap (flip (,) b) (getOp a)) ps
    a <- getReg (CPS.fv exp `mappend` Set.fromList (CPS.extractNames $ fmap fst ps))
    assoc n a
    emit (Comment $ "let " ++ n ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) ps) ++ "}")
    emit (Record ps' (r a))
    generate exp

select :: Int -> Value -> Name -> CExp -> AbstGen ()
select i v n exp = do
    o <- getOp v
    a <- getReg (CPS.fv exp)
    assoc n a
    emit (Comment $ "let " ++ n ++ " = " ++ show v ++ "[" ++ show i ++ "]")
    emit (Select i o (r a))
    generate exp

jump :: Value -> AbstGen ()
jump v = getOp v >>= emit . Jmp

-- copy ra into rb
copy :: GPR -> GPR -> AbstGen ()
copy a b = do
    desoc b
    assocd <- getAssoc a
    case assocd of
        Just v -> assoc v b
        Nothing -> pure ()
    emit (Move (r b) (r a))

-- move ra from ra into rb
move :: GPR -> GPR -> AbstGen ()
move a b = do
    copy a b
    desoc a

{-
-- uses AR, currently unused?
swap :: GPR -> GPR -> AbstGen ()
swap a b = do
    assoca <- getAssoc a
    assocb <- getAssoc b
    desoc a
    desoc b
    emit (Move ar (r a))
    emit (Move (r a) (r b))
    emit (Move (r b) ar)
    case assoca of
        Just v -> assoc b v
        Nothing -> pure ()
    case assocb of
        Just v -> assoc a v
        Nothing -> pure ()
-}

switch :: Value -> [CExp] -> AbstGen ()
switch v exps = do
    emit (Comment $ "switch " ++ show v)
    o <- getOp v
    lbls <- mapM (fmap (LocalIdentifier . ("case_"++) . show) . const caseLabel) exps
    table <- fmap (LocalIdentifier . ("table_"++) . show) caseLabel
    emit (Fetch ar (ImmLabel table) o)
    emit (Jmp ar)
    emit (Define table)
    mapM_ (emit . flip EmitPtr 0) lbls
    mapM_ (\(lbl,exp) -> do
        emit (Define lbl)
        generate exp) (zip lbls exps)

-- technically should use a bijection, can't be bothered
getMovable :: [(GPR, GPR)] -> Maybe ((GPR, GPR), [(GPR, GPR)])
getMovable xs = internal xs
    where
        occupied = fmap fst xs
        internal ((a,b):xs)
            | b `notElem` occupied = Just ((a,b), xs)
            | otherwise = fmap (second ((a,b):)) (internal xs)
        internal [] = Nothing

-- make simple, one-step moves
simpleMoves :: [(GPR, GPR)] -> AbstGen [(GPR, GPR)]
simpleMoves xs = let xs' = filterDone xs in
    case getMovable xs' of
        Just ((a,b), rem) -> do
            move a b
            simpleMoves rem
        Nothing -> pure xs'

filterDone :: [(GPR,GPR)] -> [(GPR,GPR)]
filterDone = filter (uncurry (/=))

displace :: [(GPR,GPR)] -> Maybe ((GPR,GPR), [(GPR, GPR)])
displace = uncons . filterDone

-- expects that all unused registers have been 'cleared'
permute :: [(GPR, GPR)] -> AbstGen ()
permute rs = do
    rs' <- simpleMoves rs
    case displace rs' of
        Just ((a,b), rs) -> do
            assoca <- getAssoc a
            desoc a
            emit (Move ar (r a))
            simpleMoves rs
            emit (Move (r b) ar)
            case assoca of
                Just av -> assoc av b
                Nothing -> pure ()
        Nothing -> pure ()

duplicate :: [(GPR, GPR)] -> AbstGen ()
duplicate = mapM_ (uncurry copy)

getOp :: Value -> AbstGen Operand
getOp (Lit l) = pure (ImmLit l)
getOp (Var (LocalIdentifier v)) = do
    (known, escaping, _) <- ask
    if v `Set.member` known || v `Set.member` escaping then
        pure (ImmLabel (LocalIdentifier v))
    else do
        reg <- getPrimaryReg v
        case reg of
            Just a -> pure (Reg (r a))
            Nothing -> error ("variable " ++ v ++ " unknown")

fill :: [(Operand, GPR)] -> AbstGen ()
fill = mapM_ (\(a,b) -> emit (Move (r b) a))

organizeMoves :: [(Value, GPR)] -> AbstGen ([(GPR, GPR)], [(GPR, GPR)], [(Operand, GPR)])
organizeMoves = fmap (\(a,b) -> let (x,y) = splitDuplicates (filterDone (nub a)) in (x,y,b)) . splitLits
    where
        findLast :: (a -> Bool) -> [a] -> Maybe a
        findLast f (x:xs) = case findLast f xs of
            Just x' -> Just x'
            Nothing -> if f x then Just x else Nothing
        findLast _ [] = Nothing

        splitLits :: [(Value, GPR)] -> AbstGen ([(GPR, GPR)], [(Operand, GPR)])
        splitLits [] = pure ([], [])
        splitLits ((a,b):xs) = do
            o <- getOp a
            (moves, fills) <- splitLits xs
            case o of
                Reg (GPR a') -> pure ((a',b):moves, fills)
                _ -> pure (moves, (o,b):fills)

        splitDuplicates :: [(GPR, GPR)] -> ([(GPR, GPR)], [(GPR, GPR)])
        splitDuplicates ((a,b):xs) = case findLast (\(a',_) -> a==a') xs of
            Just (_,b') -> second ((b',b):) (splitDuplicates xs)
            Nothing -> first ((a,b):) (splitDuplicates xs)
        splitDuplicates [] = ([], [])

doMoves :: [(Value, GPR)] -> AbstGen ()
doMoves xs = do
    (moves, dups, fills) <- organizeMoves xs
    permute moves
    duplicate dups
    fill fills

genLayout :: [Value] -> AbstGen [GPR]
genLayout (Var (LocalIdentifier v):xs) = do
    reg <- getPrimaryReg v
    ls <- genLayout xs
    case reg of
        Just a -> let a' = (\(Just a) -> a) $ find (not . (`elem` ls)) [0..] in
            pure (a':ls)
        Nothing -> let a' = (\(Just a) -> a) $ find (not . (`elem` ls)) [0..] in
            pure (a':ls)
genLayout (_:xs) = do
    ls <- genLayout xs
    let a = (\(Just a) -> a) $ find (not . (`elem` ls)) [0..]
    pure (a:ls)
genLayout [] = pure []

setLayout :: Name -> [Value] -> AbstGen [GPR]
setLayout n vs = do
    ls <- genLayout vs
    modify (\(a,b,c) -> (a, Map.insert n ls b, c))
    pure ls

getLayout :: Name -> [Value] -> AbstGen (Operand, [(Value, GPR)])
getLayout n vs = do
    (_, layouts, _) <- get
    layout <- case Map.lookup n layouts of
        Just layout -> pure layout
        Nothing -> setLayout n vs
    pure (ImmLabel $ LocalIdentifier n, zip vs layout)

irrefutableGetLayout :: Name -> AbstGen [GPR]
irrefutableGetLayout n = do
    (_, layouts, _) <- get
    case Map.lookup n layouts of
        Just layout -> pure layout
        Nothing -> error ("irrefutable layout lookup failed for " ++ n)

call :: Value -> [Value] -> AbstGen ()
call val@(Var (LocalIdentifier v)) args = do
    (known, _, _) <- ask
    (callOp, rmap) <- if v `Set.member` known then
            getLayout v args
        else
            pure (Reg (GPR 0), (val,0):zip args [1..])
    doMoves rmap
    emit (Comment (v ++ concatMap (\v -> ' ':show v) args))
    emit (Jmp callOp)
call (Var v) args = do
    let rmap = zip args [1..]
    doMoves rmap
    emit (Jmp (ImmLabel v))

clearRegs :: AbstGen ()
clearRegs = modify (\(_,b,c) -> (mempty,b,c))

loadRegs :: [(Name,GPR)] -> AbstGen ()
loadRegs args = modify (\(_,b,c) -> (Inj.fromList args,b,c))

partEsc :: [CFun] -> AbstGen ([CFun], [CFun])
partEsc (f@(CPS.Fun (LocalIdentifier v) _ _):fs) = do
    (nonesc, esc, _) <- ask
    (nonfn, escfn) <- partEsc fs
    if v `Set.member` nonesc then
        pure (f:nonfn, escfn)
    else if v `Set.member` esc then
        pure (nonfn, f:escfn)
    else
        error ("unknown function " ++ v)
partEsc (f:fs) = do
    (nonfn, escfn) <- partEsc fs
    pure (nonfn, f:escfn)
partEsc [] = pure ([], [])

partAssigned :: [CFun] -> AbstGen ([CFun], [CFun])
partAssigned (f@(CPS.Fun (LocalIdentifier v) _ _):fs) = do
    (_, assigned, _) <- get
    (assignedfns, otherfns) <- partAssigned fs
    if v `Map.member` assigned then
        pure (f:assignedfns, otherfns)
    else
        pure (assignedfns, f:otherfns)
partAssigned [] = pure ([], [])

genNonEsc :: [CFun] -> AbstGen ()
genNonEsc fns = do
    (assignedfns, otherfns) <- partAssigned fns
    if null assignedfns then
        -- here functions in otherfns are never called, so it is fine to halt generation
        pure ()
    else do
        mapM_ (\(CPS.Fun (LocalIdentifier n) args exp) -> do
            layout <- irrefutableGetLayout n
            emit (Comment (n ++ concatMap (\(a,i) -> ' ':a++":r"++show i) (zip args layout)))
            emit (Define (LocalIdentifier n))
            loadRegs (zip args layout)
            generate exp) assignedfns
        genNonEsc otherfns

fix :: [CFun] -> AbstGen ()
fix fns = do
    (nonesc, esc) <- partEsc fns
    mapM_ (\(CPS.Fun id args exp) -> do
        emit (Comment (show id ++ concatMap (\(a,i) -> ' ':a++":r"++show i) (zip args [1..])))
        emit (Define id)
        loadRegs (zip args [1..])
        generate exp) esc
    genNonEsc nonesc

generate :: CExp -> AbstGen ()
generate (CPS.App val args) = call val args
generate (CPS.Fix fns _) = fix fns
generate (CPS.Record paths name exp) = record paths name exp
generate (CPS.Select i v n exp) = select i v n exp
generate (CPS.Switch v exps) = switch v exps
generate (CPS.Error s) = emit (Error s)
generate CPS.Halt = emit Halt