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
import Data.List (find, uncons, nub, intercalate, partition, (\\))

import CPS.Meta

import qualified Data.Map as Map
import qualified Data.Set as Set

type AbstState = (Inj.Injection Identifier GPR, Map.Map Identifier [GPR], Int)
-- current register operand state, register layouts, current name
type AbstEnv = (Set.Set Identifier, Int)
-- escaping functions, number of registers available

runAbstGen :: [(Identifier, [GPR])] -> [Identifier] -> AbstGen a -> AbstEnv -> (a, [(Identifier,[GPR])], [Operator])
runAbstGen layouts m a e = (\(a,(_,b,_),c) -> (a,filter ((`elem` m) . fst) $ Map.toList b,c)) (runRWS a e (mempty, Map.fromList layouts, 0))

generateAbstract :: [(Identifier, [GPR])] -> [Identifier] -> CExp -> Int -> ([(Identifier,[GPR])],[Operator])
generateAbstract layouts m e n = (\(_,s,o) -> (s,fmap Exports m ++ o)) (runAbstGen layouts m (generate e) (CPS.getEscaping e,n))

type AbstGen = RWS AbstEnv [Operator] AbstState

-- observations:
-- if a called function is known, it is non-escaping
-- if a called function is unknown, it is escaping
-- escaping functions use r1 for first argument, etc.

max :: AbstGen Int
max = fmap snd ask

emit :: Operator -> AbstGen ()
emit op = tell [op]

unused :: AbstGen GPR
unused = do
    (env,_,_) <- get
    (_,n) <- ask
    let (Just a) = find (not . flip Inj.memberInv env) [0..]
    if a > n then error "NOT ENOUGH REGISTERS SPILLS NOT YET IMPLEMENTED" else pure ()
    pure a

allUnused :: AbstGen [GPR]
allUnused = do
    (env,_,_) <- get
    (_,n) <- ask
    pure $ filter (not . flip Inj.memberInv env) [0..n-1]

cull :: Set.Set Identifier -> AbstGen ()
cull free = modify (\(a,b,c) -> (Inj.filterFst (`Set.member` free) a, b, c))

getReg :: Set.Set Identifier -> AbstGen GPR
getReg free = do
    cull free
    unused

caseLabel :: AbstGen Int
caseLabel = do
    (a,b,c) <- get
    put (a,b,c+1)
    pure c

assoc :: Identifier -> GPR -> AbstGen ()
assoc n r = modify (\(a,b,c) -> (Inj.insert n r a, b, c))

getAssoc :: GPR -> AbstGen (Maybe Identifier)
getAssoc r = do
    (env, _, _) <- get
    pure . fmap fst $ Inj.lookupInv r env

getPrimaryReg :: Identifier -> AbstGen (Maybe GPR)
getPrimaryReg v = do
    (env, _, _) <- get
    case Inj.lookup v env of
        ((_, x):_) -> pure $ Just x
        _ -> pure Nothing

desoc :: GPR -> AbstGen ()
desoc r = modify (\(a,b,c) -> (Inj.deleteSnd r a, b, c))

keep :: [GPR] -> AbstGen ()
keep rs = modify (\(a,b,c) -> (Inj.filterSnd (`elem` rs) a, b, c))

record :: [(Value, AccessPath)] -> Identifier -> CExp -> AbstGen ()
record ps n exp = do
    ps' <- mapM (\(a,b) -> fmap (flip (,) b) (getOp a)) ps
    a <- getReg (CPS.fv exp `mappend` Set.fromList (CPS.extractIdents $ fmap fst ps))
    assoc n a
    emit (Comment $ "let " ++ show n ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) ps) ++ "} (in r" ++ show a ++ ")")
    emit (Record ps' (r a))
    generate exp

primop :: Primop -> [Value] -> Identifier -> [CExp] -> AbstGen ()
primop op vs n [exp] | effectOp op = do
    os <- mapM getOp vs
    a <- getReg (CPS.fv exp `mappend` Set.fromList (CPS.extractIdents vs))
    assoc n a
    emit (EffectOp op os)
    emit (Move (GPR a) (ImmLit (Int 0)))
    generate exp
primop op vs n [exp] | dataOp op = do
    os <- mapM getOp vs
    a <- getReg (CPS.fv exp `mappend` Set.fromList (CPS.extractIdents vs))
    assoc n a
    emit (DataOp op (GPR a) os)
    generate exp
primop op vs _ exps | switchOp op = do
    vs' <- mapM getOp vs
    -- using the placeholder register
    a <- getReg (mconcat (fmap CPS.fv exps) `mappend` Set.fromList (CPS.extractIdents vs))
    lbls <- mapM (fmap (LocalIdentifier . Gen "switch_") . const caseLabel) exps
    emit (SwitchOp op (GPR a) vs' (fmap ImmLabel lbls))
    emit (Jmp (r a))
    (regs,_,_) <- get
    mapM_ (\(lbl,exp) -> do
        modify (\(_,b,c) -> (regs,b,c))
        emit (Define lbl)
        generate exp) (zip lbls exps)

select :: Int -> Value -> Identifier -> CExp -> AbstGen ()
select i v n exp = do
    o <- getOp v
    a <- getReg (CPS.fv exp)
    assoc n a
    emit (Comment $ "let " ++ show n ++ " = " ++ show v ++ "[" ++ show i ++ "] (in r"++ show a ++ ")")
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
    lbls <- mapM (fmap (LocalIdentifier . Gen "case_") . const caseLabel) exps
    table <- fmap (LocalIdentifier . Gen "table_") caseLabel
    emit (Fetch ar (ImmLabel table) o)
    emit (Jmp ar)
    emit (Table table (fmap ImmLabel lbls))
    (regs,_,_) <- get
    mapM_ (\(lbl,exp) -> do
        modify (\(_,b,c) -> (regs,b,c))
        emit (Define lbl)
        generate exp) (zip lbls exps)

orderMoveable :: [(GPR,GPR)] -> ([(GPR,GPR)],[(GPR,GPR)])
orderMoveable [] = ([],[])
orderMoveable xs =
    let srcs = fmap fst xs
        (moveable,rem) = partition ((`notElem` srcs) . snd) xs
    in case moveable of
        []  -> ([],rem)
        _   -> first (moveable++) (orderMoveable rem)

permute :: [(GPR,GPR)] -> AbstGen ()
permute xs | all (`notElem` fmap snd xs) (fmap fst xs) = mapM_ (uncurry move) xs
permute xs = do
        let ((s,d):xs') = xs
        emit (Move Arith (r s))
        let (m,nm) = orderMoveable xs'
        mapM_ (uncurry move) m
        emit (Move (r d) ar)
        permute nm

duplicate :: [(GPR, GPR)] -> AbstGen ()
duplicate = mapM_ (uncurry copy)

getOp :: Value -> AbstGen Operand
getOp (Lit l) = pure (ImmLit l)
getOp (Var v) = do
    reg <- getPrimaryReg v
    case reg of
        Just a -> pure (Reg (r a))
        Nothing -> do
            (env,_,_) <- get
            error ("variable " ++ show v ++ " unknown in env " ++ show env)
getOp (Label i) = pure (ImmLabel i)

fill :: [(Operand, GPR)] -> AbstGen ()
fill = mapM_ (\(a,b) -> emit (Move (r b) a))

organizeMoves :: [(Value, GPR)] -> AbstGen ([(GPR, GPR)], [(GPR, GPR)], [(Operand, GPR)])
organizeMoves = fmap (\(a,b) -> let (x,y) = splitDuplicates (filter (uncurry (/=)) (nub a)) in (x,y,b)) . splitLits
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
    cull (Set.fromList . CPS.extractIdents $ fmap fst xs)
    (moves, dups, fills) <- organizeMoves xs
    freeregs <- allUnused
    emit (Comment $ "moves:" ++ concatMap (\(i,o)->" (r" ++ show i ++ " -> r" ++ show o ++ ")") moves)
    emit (Comment $ "dups:" ++ concatMap (\(i,o)->" (r" ++ show i ++ " -> r" ++ show o ++ ")") dups)
    emit (Comment $ "free:" ++ concatMap ((' ':) . ('r':) . show) freeregs)
    permute moves
    duplicate dups
    fill fills

genLayout :: [Value] -> AbstGen [GPR]
genLayout (Var v:xs) = do
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

saveLayout :: Identifier -> [GPR] -> AbstGen ()
saveLayout n ls = modify (\(a,b,c) -> (a, Map.insert n ls b, c))

setLayout :: Identifier -> [Value] -> AbstGen [GPR]
setLayout n vs = do
    ls <- genLayout vs
    saveLayout n ls
    pure ls

getLayout :: Identifier -> [Value] -> AbstGen [(Value, GPR)]
getLayout n vs = do
    (_, layouts, _) <- get
    layout <- case Map.lookup n layouts of
        Just layout -> pure layout
        Nothing -> setLayout n vs
    pure (zip vs layout)

irrefutableGetLayout :: Identifier -> AbstGen [GPR]
irrefutableGetLayout n = do
    (_, layouts, _) <- get
    case Map.lookup n layouts of
        Just layout -> pure layout
        Nothing -> error ("irrefutable layout lookup failed for " ++ show n)

call :: Value -> [Value] -> AbstGen ()
call val@(Var v) args = do
    doMoves ((val,0):zip args [1..])
    emit (Comment (show v ++ concatMap (\v -> ' ':show v) args))
    emit (Jmp (Reg (GPR 0)))
call (Label v) args = do
    rmap <- getLayout v args
    doMoves rmap
    emit (Comment (show (Label v) ++ concatMap (\(v,i) -> ' ':show v++":r"++show i) rmap))
    emit (Jmp (ImmLabel v))

clearRegs :: AbstGen ()
clearRegs = modify (\(_,b,c) -> (mempty,b,c))

loadRegs :: [(Identifier,GPR)] -> AbstGen ()
loadRegs args = modify (\(_,b,c) -> (Inj.fromList args,b,c))

partEsc :: [CFun] -> AbstGen ([CFun], [CFun])
partEsc (f@(CPS.Fun _ v _ _):fs) = do
    (esc, _) <- ask
    (nonfn, escfn) <- partEsc fs
    if v `Set.member` esc then
        pure (nonfn, f:escfn)
    else
        pure (f:nonfn, escfn)
partEsc [] = pure ([], [])

partAssigned :: [CFun] -> AbstGen ([CFun], [CFun])
partAssigned (f@(CPS.Fun _ v _ _):fs) = do
    (_, assigned, _) <- get
    (assignedfns, otherfns) <- partAssigned fs
    if v `Map.member` assigned then
        pure (f:assignedfns, otherfns)
    else
        pure (assignedfns, f:otherfns)
partAssigned [] = pure ([], [])

genEsc :: CFun -> AbstGen ()
genEsc (CPS.Fun _ id args exp) = do
    emit (Comment (show id ++ concatMap (\(a,i) -> ' ':show a++":r"++show i) (zip args [1..])))
    emit (Define id)
    loadRegs (zip args [1..])
    saveLayout id (take (length args) [1..])
    generate exp

genNonAssigned :: [CFun] -> AbstGen ()
genNonAssigned (f:fs) = genEsc f >> genNonEsc fs

genNonEsc :: [CFun] -> AbstGen ()
genNonEsc [] = pure ()
genNonEsc fns = do
    (assignedfns, otherfns) <- partAssigned fns
    if null assignedfns then
        genNonAssigned otherfns
    else do
        mapM_ (\(CPS.Fun _ n args exp) -> do
            layout <- irrefutableGetLayout n
            emit (Comment (show n ++ concatMap (\(a,i) -> ' ':show a++":r"++show i) (zip args layout)))
            emit (Define n)
            loadRegs (zip args layout)
            generate exp) assignedfns
        genNonEsc otherfns

fix :: [CFun] -> AbstGen ()
fix fns = do
    (nonesc, esc) <- partEsc fns
    mapM_ genEsc esc
    genNonEsc nonesc

generate :: CExp -> AbstGen ()
generate (CPS.App val args) = call val args
generate (CPS.Fix fns _) = fix fns
generate (CPS.Record paths name exp) = record paths name exp
generate (CPS.Select i v n exp) = select i v n exp
generate (CPS.Switch v exps) = switch v exps
generate (CPS.Error s) = emit (Error s)
generate (CPS.Primop p vs i exps) = primop p vs i exps
generate CPS.Halt = emit Halt