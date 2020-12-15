module CPS.Interpreter (interpret) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Exception

import Types.Ident
import Types.CPS
import Types.Prim

import Types.Build

import qualified Data.Map as Map
import Data.List
import Data.Char

type GlobalIdent = (ModulePath,Identifier)

data CVal
    = CRecord [CVal]
    | CLit UnboxedLit
    | CPtr GlobalIdent
    deriving(Show)

type CPSEnv = (ModulePath, [GlobalIdent], Map.Map GlobalIdent ([Identifier], CExp), Map.Map GlobalIdent CVal)

printEnv :: Interpreter ()
printEnv = do
    (p,trace,fns,mem) <- ask
    liftIO . putStrLn $ "trace: " ++ intercalate " <- " (fmap (\(m,i) -> intercalate "." m ++ " " ++ show i) trace)
    liftIO . putStrLn $ "in module: " ++ intercalate "." p
    liftIO . forM_ (Map.toList fns) $ \((m,i),(a,e)) -> do
        putStrLn $ intercalate "." m ++ " " ++ show i ++ concatMap ((' ':) . show) a ++ ": "
        print e
    liftIO . forM_ (Map.toList mem) $ \((m,i),v) -> do
        putStr $ intercalate "." m ++ " " ++ show i ++ ": "
        print v
    

type Interpreter = ReaderT CPSEnv IO

runInterpreter :: Interpreter a -> CPSEnv -> IO a
runInterpreter = runReaderT

interpret :: Identifier -> [(ModulePath,CExp)] -> IO ()
interpret i es = runInterpreter (interpretExp (App (Label i) [])) (toEnv [] es)

toEnv :: ModulePath -> [(ModulePath,CExp)] -> CPSEnv
toEnv p fs = (p,[],Map.fromList (fns fs),mempty)
    where
        fns ((p,Fix defs _):xs) =
            fmap (\(Fun n args exp) -> ((p,n),(args,exp))) defs
            ++ fns xs
        fns [] = []

getGlobal :: Identifier -> Interpreter GlobalIdent
getGlobal i = do
    (p,_,_,_) <- ask
    pure (p,i)

lookupVar :: GlobalIdent -> Interpreter CVal
lookupVar i = do
    (_,_,_,vs) <- ask
    case Map.lookup i vs of
        Just v -> pure v
        Nothing -> do
            fatal $ "value " ++ show i ++ " not found"
            undefined

convVal :: Value -> Interpreter CVal
convVal (Label i@(ExternalIdentifier m _)) = pure (CPtr (m,i))
convVal (Label i) = CPtr <$> getGlobal i
convVal (Var i) = lookupVar =<< getGlobal i
convVal (Lit l) = pure (CLit l)

enterModule :: ModulePath -> Interpreter a -> Interpreter a
enterModule p = local (\(_,t,a,b)->(p,t,a,b))

withVars :: [(Identifier,CVal)] -> Interpreter a -> Interpreter a
withVars vs c = do
    vs' <- mapM (\(b,a) -> fmap (flip (,) a) (getGlobal b)) vs
    local (\(m,t,a,b)->(m,t,a,Map.fromList vs' `mappend` b)) c

withTrace :: GlobalIdent -> Interpreter a -> Interpreter a
withTrace i = local (\(p,t,a,b)->(p,i:t,a,b))

execGlobal :: GlobalIdent -> [CVal] -> Interpreter ()
execGlobal g@(m,_) vs = do
    (_,_,fns,_) <- ask
    case Map.lookup g fns of
        Just (args, exp) -> do
            if length args /= length vs then
                liftIO . putStrLn $ "WARNING: arguments " ++ show args ++ " given values: " ++ show vs
            else pure ()
            enterModule m . withTrace g $ withVars (zip args vs) (interpretExp exp)
        Nothing -> fatal $ "function " ++ show g ++ " not found"

access :: CVal -> AccessPath -> CVal
access v NoPath = v
access (CRecord vs) (SelPath i p) = access (vs !! i) p

getArithOp :: Primop -> Int -> Int -> Int
getArithOp AAdd = (+)
getArithOp ASub = (-)
getArithOp AMul = (*)
getArithOp ADiv = div

fatal :: String -> Interpreter ()
fatal s = do
    printEnv
    liftIO $ putStrLn s
    pure ()

getIOOp :: Primop -> CVal -> Interpreter ()
getIOOp PutChr (CLit (Char c)) = liftIO $ putChar c
getIOOp PutInt (CLit (Int i)) = liftIO $ putStr (show i)
getIOOp p v = fatal $ "tried to " ++ show p ++ " " ++ show v

getCoerceOp :: Primop -> CVal -> Interpreter CVal
getCoerceOp IntToChar (CLit (Int i)) = pure (CLit . Char $ chr i)
getCoerceOp CharToInt (CLit (Char c)) = pure (CLit . Int $ ord c)
getCoerceOp p v = fatal ("tried to " ++ show p ++ " " ++ show v) >> undefined

interpretExp :: CExp -> Interpreter ()
interpretExp (App v vs) = do
    v' <- convVal v
    case v' of
        CPtr g -> execGlobal g =<< mapM convVal vs
        _ -> fatal $ "tried to jump to " ++ show v'
interpretExp (Record vp n c) = do
    vs <- mapM (\(v,p) -> access <$> convVal v <*> pure p) vp
    withVars [(n,CRecord vs)] (interpretExp c)
interpretExp (Select i v n c) = do
    v' <- convVal v
    withVars [(n,access v' (SelPath i NoPath))] (interpretExp c)
interpretExp (Switch v cs) = do
    v' <- convVal v
    case v' of
        CLit (Int i) -> interpretExp (cs !! i)
        _ -> fatal $ "tried to switch on " ++ show v'
interpretExp (Primop p [a,b] n [c]) | arithOp p = do
    a' <- convVal a
    b' <- convVal b
    case (a',b') of
        (CLit (Int a),CLit (Int b)) -> withVars [(n,CLit (Int (getArithOp p a b)))] (interpretExp c)
        _ -> fatal $ "tried to " ++ show p ++ " " ++ show (a',b')
interpretExp (Primop p [v] n [c]) | effectOp p = do
    v' <- convVal v
    getIOOp p v'
    withVars [(n,CLit (Int 0))] (interpretExp c)
interpretExp (Primop p [v] n [c]) | coerceOp p = do
    v' <- getCoerceOp p =<< convVal v
    withVars [(n,v')] (interpretExp c)
interpretExp (Primop CmpInt [a,b] _ [c,d,e]) = do
    a' <- convVal a
    b' <- convVal b
    case (a',b') of
        (CLit (Int av),CLit (Int bv)) | av == bv -> interpretExp c
        (CLit (Int av),CLit (Int bv)) | av > bv -> interpretExp d
        (CLit (Int av),CLit (Int bv)) | av < bv -> interpretExp e
        _ -> fatal $ "tried to integer compare " ++ show (a',b')
interpretExp (Error s) = liftIO . putStr $ "ERROR: " ++ s
interpretExp Halt = pure ()