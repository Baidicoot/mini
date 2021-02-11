{-# LANGUAGE LambdaCase #-}

module Backend.Interpreter where

import Control.Monad.Reader
import Types.Abstract
import Types.Ident
import Types.Prim
import Types.Build
import qualified Data.Map as Map
import Data.List
import Data.Char
import Types.CPS (AccessPath(..))

data CVal
    = CRecord [CVal]
    | CLit UnboxedLit
    | CPtr GlobalIdent
    deriving(Show)

type GlobalIdent = (ModulePath,Identifier)

showGlob :: GlobalIdent -> String
showGlob (p,i) = intercalate "." p ++ " " ++ show i

type InterpState = (ModulePath, [GlobalIdent], Map.Map GlobalIdent [CVal], Map.Map GlobalIdent [Operator], [((CVal,GlobalIdent),[(CVal,GlobalIdent)])], CVal)

type Interpreter = ReaderT InterpState Build

interpret :: [(ModulePath, [Operator])] -> Int -> ModulePath -> Identifier -> Build ()
interpret c r mp id = runReaderT (execGlobal (mp,id)) (mkState c r (mp,id))

moveStatic :: [Operator] -> ([Operator],[Operator])
moveStatic = partition (\case
    Table _ _ -> True
    _ -> False)

partitionLabels :: ModulePath -> [Operator] -> Map.Map GlobalIdent [Operator]
partitionLabels p (Define i:s) =
    let def = takeWhile (not . isDef) s
        rem = partitionLabels p (dropWhile (not . isDef) s)
    in Map.insert (p,i) def rem
    where
        isDef (Define _) = True
        isDef _ = False
partitionLabels p (_:xs) = partitionLabels p xs
partitionLabels _ [] = mempty

mkState :: [(ModulePath, [Operator])] -> Int -> GlobalIdent -> InterpState
mkState = internal mempty mempty
    where
        internal :: Map.Map GlobalIdent [CVal] -> Map.Map GlobalIdent [Operator] -> [(ModulePath, [Operator])] -> Int -> GlobalIdent -> InterpState
        internal tbls fns ((p,ops):xs) r g =
            let (mtb,mops) = moveStatic ops
                mfns = partitionLabels p mops
                mtbls = Map.fromList (fmap (\(Table l vs) -> ((p,l),fmap (convOperConst p) vs)) mtb)
            in internal (mtbls `mappend` tbls) (mfns `mappend` fns) xs r g
        internal tbls fns [] r g@(m,_) = (m, [], tbls, fns, replicate r ((CLit (Int 0),g),[]),CLit (Int 0))

printEnv :: Interpreter ()
printEnv = do
    (p,trace,tab,fns,regs,_) <- ask
    liftIO . putStrLn $ "trace: " ++ intercalate " <- " (fmap (\(m,i) -> intercalate "." m ++ " " ++ show i) trace)
    liftIO . putStrLn $ "in module: " ++ intercalate "." p
    liftIO . forM_ (zip [0..] regs) $ \(i,((v,g),p)) -> do
        putStrLn $ show i ++ ": " ++ show v ++ "; set in " ++ showGlob g
        forM_ p $ \(v,g) -> putStrLn $ "    previously " ++ show v ++ "; set in " ++ showGlob g

convOperConst :: ModulePath -> Operand -> CVal
convOperConst m (ImmLabel l) = CPtr (m,l)
convOperConst m (ImmLit l) = CLit l

convOper :: Operand -> Interpreter CVal
convOper (Reg (GPR i)) = asks (fst . fst . (\(_,_,_,_,rs,_)->rs !! i))
convOper (Reg Arith) = asks (\(_,_,_,_,_,ar)->ar)
convOper (ImmLabel l) = CPtr <$> getGlobalLabel l
convOper (ImmLit l) = pure (CLit l)

getGlobalLabel :: Identifier -> Interpreter GlobalIdent
getGlobalLabel i@(LocalIdentifier _) = do
    (p,_,_,_,_,_) <- ask
    pure (p,i)
getGlobalLabel i@(ExternalIdentifier m _) = pure (m,i)

enterModule :: ModulePath -> Interpreter a -> Interpreter a
enterModule p = local (\(_,tr,ta,a,b,ar)->(p,tr,ta,a,b,ar))

replace :: Int -> a -> [(a,[a])] -> [(a,[a])]
replace 0 v ((p,pp):xs) = (v,p:pp):xs
replace n v (x:xs) = x:replace (n-1) v xs
replace n v [] = []

withGPR :: Int -> CVal -> Interpreter a -> Interpreter a
withGPR i v = local (\(p,tr@(g:_),ta,a,b,ar)->(p,tr,ta,a,replace i (v,g) b,ar))

withReg :: Register -> CVal -> Interpreter a -> Interpreter a
withReg Arith v = local (\(p,tr,ta,a,b,_)->(p,tr,ta,a,b,v))
withReg (GPR i) v = withGPR i v

withTrace :: GlobalIdent -> Interpreter a -> Interpreter a
withTrace i = local (\(p,tr,ta,a,b,ar)->(p,i:tr,ta,a,b,ar))

fatal :: String -> Interpreter ()
fatal s = do
    printEnv
    liftIO $ putStrLn s
    pure ()

access :: CVal -> AccessPath -> Interpreter CVal
access v NoPath = pure v
access (CRecord vs) (SelPath i p) = access (vs !! i) p
access v p = fatal ("tried to access component " ++ show p ++ " of " ++ show v) >> undefined

getArithOp :: Primop -> Int -> Int -> Int
getArithOp AAdd = (+)
getArithOp ASub = (-)
getArithOp AMul = (*)
getArithOp ADiv = div

getIOOp :: Primop -> CVal -> Interpreter ()
getIOOp PutChr (CLit (Char c)) = liftIO $ putChar c
getIOOp PutInt (CLit (Int i)) = liftIO $ putStr (show i)
getIOOp p v = fatal $ "tried to " ++ show p ++ " " ++ show v

getCoerceOp :: Primop -> CVal -> Interpreter CVal
getCoerceOp IntToChar (CLit (Int i)) = pure (CLit . Char $ chr i)
getCoerceOp CharToInt (CLit (Char c)) = pure (CLit . Int $ ord c)
getCoerceOp p v = fatal ("tried to " ++ show p ++ " " ++ show v) >> undefined

execGlobal :: GlobalIdent -> Interpreter ()
execGlobal g@(m,_) = do
    (_,_,_,lbls,_,_) <- ask
    case Map.lookup g lbls of
        Just ops -> enterModule m . withTrace g $ interpretOps ops
        Nothing -> fatal $ "function " ++ show g ++ " not found"

interpretOps :: [Operator] -> Interpreter ()
interpretOps (DataOp p r [o1,o2]:k) = do
    a' <- convOper o1
    b' <- convOper o2
    case (a',b') of
        (CLit (Int a),CLit (Int b)) -> withReg r (CLit . Int $ getArithOp p a b) (interpretOps k)
interpretOps (EffectOp p [o]:k) = do
    getIOOp p =<< convOper o
    interpretOps k
interpretOps (DataOp p r [o]:k) = do
    v <- getCoerceOp p =<< convOper o
    withReg r v (interpretOps k)
interpretOps (SwitchOp CmpInt r [a,b] [eq,gt,lt]:k) = do
    a' <- convOper a
    b' <- convOper b
    case (a',b') of
        (CLit (Int a),CLit (Int b)) | a == b -> (\e -> withReg r e (interpretOps k)) =<< convOper eq
        (CLit (Int a),CLit (Int b)) | a > b -> (\g -> withReg r g (interpretOps k)) =<< convOper gt
        (CLit (Int a),CLit (Int b)) | a < b -> (\l -> withReg r l (interpretOps k)) =<< convOper lt
        _ -> fatal $ "tried to integer compare " ++ show (a',b')
interpretOps (Jmp o:_) = do
    o' <- convOper o
    case o' of
        (CPtr g) -> execGlobal g
        _ -> fatal $ "tried to jump to " ++ show o' ++ " (" ++ show o ++ ")"
interpretOps (Record vp r:k) = do
    vs <- mapM (\(v,p) -> (`access` p) =<< convOper v) vp
    withReg r (CRecord vs) (interpretOps k)
interpretOps (Select i v r:k) = do
    v' <- (`access` SelPath i NoPath) =<< convOper v
    withReg r v' (interpretOps k)
interpretOps (Fetch r o1 o2:k) = do
    l <- convOper o1
    o <- convOper o2
    tbls <- asks (\(_,_,t,_,_,_)->t)
    case (l,o) of
        (CPtr g, CLit (Int i)) -> case Map.lookup g tbls of
            Just x -> withReg r (x !! i) (interpretOps k)
            Nothing -> fatal $ "table " ++ show g ++ " does not exist"
        _ -> fatal $ "tried to access table with " ++ show (l,o)
interpretOps (Move r o:k) = do
    o <- convOper o
    withReg r o (interpretOps k)
interpretOps (Error s:_) = liftIO . putStr $ "ERROR: " ++ s
interpretOps (Halt:_) = pure ()
interpretOps (_:xs) = interpretOps xs
interpretOps [] = fatal "instruction underflow"

operInterp :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
operInterp BuildConfig{backend = Backend _ r s} xs glue = do
    let fs = ([],glue):fmap (\(p,Right ops)->(p,ops)) xs
    -- liftIO $ forM_ fs $ \(p,ops) -> writeFile (root ++ "test." ++ intercalate "." p ++ ".txt") (show ops)
    liftIO $ putStr "\n"
    interpret fs r [] s

interpreter :: Int -> Backend
interpreter r = Backend operInterp r (LocalIdentifier "start")