module Types.Module where

import Types.Ident
import Types.Type
import Types.Graph
import Types.Abstract

import Data.List

import Control.Arrow
import Control.Monad
import Control.Monad.Errors

mainFn :: ModulePath -> Identifier
mainFn p = ExternalIdentifier p "main"

data ModuleABI = ModuleABI
    { moduleABIPath :: ModulePath
    , moduleABIMain :: Identifier
    , moduleABIReqs :: [ModulePath]
    , moduleABIRegs :: [GPR]
    } deriving(Show)

data ModuleAPI = ModuleAPI
    { moduleAPIPath :: ModulePath
    , moduleAPITerms :: [(Name, Scheme)]
    , moduleAPITypes :: [(Name, Kind)]
    , moduleAPIGADTs :: [GADT]
    } deriving(Show)

data GADT = GADT
    { gadtName :: Identifier
    , gadtKind :: Kind
    , gadtCons :: [(Identifier, Scheme)]
    } deriving(Show)

data ModuleServer = ModuleServer
    { abis :: [ModuleABI]
    , apis :: [ModuleAPI]
    , gadts :: [GADT]
    } deriving(Show)

constructAPI :: ModulePath -> [(Name, Scheme)] -> [GADT] -> ModuleAPI
constructAPI p ns gs =
    let gadtkinds = fmap (discardPath . gadtName &&& gadtKind) gs
        constypes = concatMap (fmap (first discardPath) . gadtCons) gs
    in ModuleAPI p (ns ++ constypes) gadtkinds gs

emptyServer :: ModuleServer
emptyServer = ModuleServer mempty mempty mempty

termTypes :: ModuleServer -> [(Identifier, Scheme)]
termTypes (ModuleServer _ apis _) = concatMap (\(ModuleAPI p t _ _) -> fmap (first (ExternalIdentifier p)) t) apis

typeKinds :: ModuleServer -> [(Identifier, Kind)]
typeKinds (ModuleServer _ apis _) = concatMap (\(ModuleAPI p _ t _) -> fmap (first (ExternalIdentifier p)) t) apis

consTypes :: ModuleServer -> [(Identifier,Scheme)]
consTypes = concatMap gadtCons . gadts

regLayouts :: ModuleServer -> [(Identifier,[GPR])]
regLayouts = fmap (\(ModuleABI p i _ l) -> (i, l)) . abis

getABI :: ModuleServer -> ModulePath -> Maybe ModuleABI
getABI m i = internal i (abis m)
    where
        internal i (x:xs)
            | i == moduleABIPath x = Just x
            | otherwise = internal i xs
        internal i [] = Nothing

getAPI :: ModuleServer -> ModulePath -> Maybe ModuleAPI
getAPI m i = internal i (apis m)
    where
        internal i (x:xs)
            | i == moduleAPIPath x = Just x
            | otherwise = internal i xs
        internal i [] = Nothing

loadModule :: ModuleABI -> ModuleAPI -> ModuleServer -> ModuleServer
loadModule abi api@(ModuleAPI _ _ _ g) (ModuleServer abs aps as) = ModuleServer (abi:abs) (api:aps) (g ++ as)

loadAPI :: ModuleAPI -> ModuleServer -> ModuleServer
loadAPI api@(ModuleAPI _ _ _ g) (ModuleServer abs aps as) = ModuleServer abs (api:aps) (g ++ as)

getSignature :: ModuleAPI -> Type
getSignature (ModuleAPI _ ts _ _) =
    let --a = mconcat $ fmap (\(_,Forall a _) -> a) ts
        t = Node NoTag . Product $ fmap (\(_,Forall _ t) -> t) ts
    in t

getGADT :: ModuleServer -> Identifier -> Maybe GADT
getGADT m i = internal i (gadts m)
    where
        internal i (x:xs)
            | i == gadtName x = Just x
            | otherwise = internal i xs
        internal i [] = Nothing

lengthGADT :: GADT -> Int
lengthGADT = length . gadtCons

indexGADT :: GADT -> Identifier -> Maybe Int
indexGADT g i = snd <$> find ((==i) . fst . fst) (zip (gadtCons g) [0..])

data ImportAction
    = ImportAs ModulePath
    deriving(Eq, Show)

data Env = Env
    { termRenames :: [(Identifier, Identifier)]
    , typeRenames :: [(Identifier, Identifier)]
    , constructors :: [Identifier]
    }

emptyEnv :: Env
emptyEnv = Env mempty mempty mempty

data ImportError
    = NameCollision ModulePath Identifier (Identifier, Identifier)
    | UnfulfilledDependency [ModulePath]

instance Show ImportError where
    show (NameCollision p i (a,b)) = "module " ++ intercalate "." p ++ ": the name `" ++ show i ++ "` could refer to either `" ++ show a ++ "` or `" ++ show b ++ "`"
    show (UnfulfilledDependency mp) = "could not resolve a dependency ordering for:" ++ concatMap (\p -> "\n    " ++ intercalate "." p) mp

sortDependencies :: [(ModulePath,[ModulePath],a)] -> Either [ImportError] [(ModulePath,a)]
sortDependencies = internal []
    where
        internal :: [(ModulePath,a)] -> [(ModulePath,[ModulePath],a)] -> Either [ImportError] [(ModulePath,a)]
        internal sorted [] = pure sorted
        internal sorted todo = 
            let (solved, remaining) = partition (satisfiedDependencies sorted) todo
            in case solved of
                [] -> Left [UnfulfilledDependency $ fmap fst sorted ++ concatMap (\(_,p,_)->p) remaining]
                _ -> internal (fmap (\(a,_,b)->(a,b)) solved ++ sorted) remaining 

        satisfiedDependencies :: [(ModulePath,a)] -> (ModulePath,[ModulePath],a) -> Bool
        satisfiedDependencies has (_,m,_) = null (m \\ fmap fst has)

checkRenames :: ModulePath -> [(Identifier, Identifier)] -> [(Identifier, Identifier)] -> Errors [ImportError] [(Identifier, Identifier)]
checkRenames path p p' = case partition ((`elem` fmap fst p) . fst) p' of
    ([], xs) -> pure (p ++ xs)
    (err,_) -> throw $ fmap (\(a,b) -> let (Just c) = lookup a p in NameCollision path a (b,c)) err

importWithAction :: ModuleAPI -> ImportAction -> Env -> Errors [ImportError] Env
importWithAction (ModuleAPI p mtr mty gs) (ImportAs p') (Env tr ty co) =
    let rtr = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mtr]
        rty = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mty]
        rco = [n | GADT _ _ co <- gs, n <- fmap fst co]
        tr' = checkRenames p tr rtr
        ty' = checkRenames p ty rty
        co' = pure $ nub (rco ++ co)
    in liftM3 Env tr' ty' co'

doImports :: ModuleServer -> [(ModulePath,ImportAction)] -> Errors [ImportError] Env
doImports ms [] = pure emptyEnv
doImports ms ((mp,ia):is) = case getAPI ms mp of
    Just api -> importWithAction api ia =<< doImports ms is
    Nothing -> throw [UnfulfilledDependency [mp]]