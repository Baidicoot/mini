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
    , moduleAPICons :: [(Name,Scheme)]
    , moduleAPITypes :: [(Name, Kind)]
    , moduleAPIGADTs :: [GADT]
    , moduleAPIEqtns :: [Eqtn]
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
    , eqtns :: [Eqtn]
    } deriving(Show)

constructAPI :: ModulePath -> [(Name, Scheme)] -> [GADT] -> [Eqtn] -> ModuleAPI
constructAPI p ns gs eq =
    let gadtkinds = fmap (discardPath . gadtName &&& gadtKind) gs
        constypes = concatMap (fmap (first discardPath) . gadtCons) gs
    in ModuleAPI p ns constypes gadtkinds gs eq

emptyServer :: ModuleServer
emptyServer = ModuleServer mempty mempty mempty mempty

termTypes :: ModuleServer -> [(Identifier, Scheme)]
termTypes (ModuleServer _ apis _ _) = concatMap (\(ModuleAPI p t c _ _ _) -> fmap (first (ExternalIdentifier p)) (t++c)) apis

typeKinds :: ModuleServer -> [(Identifier, Kind)]
typeKinds (ModuleServer _ apis _ _) = concatMap (\(ModuleAPI p _ _ t _ _) -> fmap (first (ExternalIdentifier p)) t) apis

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
loadModule abi api@(ModuleAPI _ _ _ _ g e) (ModuleServer abs aps as eq) = ModuleServer (abi:abs) (api:aps) (g ++ as) (e ++ eq)

loadAPI :: ModuleAPI -> ModuleServer -> ModuleServer
loadAPI api@(ModuleAPI _ _ _ _ g e) (ModuleServer abs aps as eq) = ModuleServer abs (api:aps) (g ++ as) (e ++ eq)

getSignature :: ModuleAPI -> Type
getSignature (ModuleAPI _ t _ _ _ _) =
    let --a = mconcat $ fmap (\(_,Forall a _) -> a) ts
        sig = Node NoTag . Product $ fmap (\(_,Forall _ t) -> t) t
    in sig

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
    | Include
    deriving(Eq, Show)

data Env = Env
    { termRenames :: [(Identifier, Identifier)]
    , typeRenames :: [(Identifier, Identifier)]
    , consRenames :: [(Identifier, Identifier)]
    }

emptyEnv :: Env
emptyEnv = Env mempty mempty mempty

data ImportError
    = NameCollision ModulePath Identifier (Identifier, Identifier)
    | NonexistentModule ModulePath
    | UnfulfilledDependency [(ModulePath,[ModulePath])]

instance Show ImportError where
    show (NameCollision p i (a,b)) = "module " ++ intercalate "." p ++ ": the name `" ++ show i ++ "` could refer to either `" ++ show a ++ "` or `" ++ show b ++ "`"
    show (UnfulfilledDependency mp) = "could not resolve a dependency ordering for:" ++ concatMap (\(p,rq) -> "\n    " ++ intercalate "." p ++ " which requires" ++ concatMap (\p -> " " ++ intercalate "." p) rq) mp
    show (NonexistentModule m) = "could not find the module " ++ intercalate "." m

sortDependencies :: [(ModulePath,[ModulePath],a)] -> Either [ImportError] [(ModulePath,a)]
sortDependencies = fmap (fmap (\(a,_,b)->(a,b))) . internal []
    where
        internal :: [(ModulePath,[ModulePath],a)] -> [(ModulePath,[ModulePath],a)] -> Either [ImportError] [(ModulePath,[ModulePath],a)]
        internal sorted [] = pure sorted
        internal sorted todo = 
            let (solved, remaining) = partition (satisfiedDependencies sorted) todo
            in case solved of
                [] -> Left [UnfulfilledDependency $ fmap (\(a,b,_)->(a,b)) (solved ++ sorted ++ remaining)]
                _ -> internal (sorted ++ solved) remaining 

        satisfiedDependencies :: [(ModulePath,[ModulePath],a)] -> (ModulePath,[ModulePath],a) -> Bool
        satisfiedDependencies has (_,m,_) = null (m \\ fmap (\(a,_,_)->a) has)

checkRenames :: ModulePath -> [(Identifier, Identifier)] -> [(Identifier, Identifier)] -> Errors [ImportError] [(Identifier, Identifier)]
checkRenames path p p' = case partition ((`elem` fmap fst p) . fst) p' of
    ([], xs) -> pure (p ++ xs)
    (err,_) -> throw $ fmap (\(a,b) -> let (Just c) = lookup a p in NameCollision path a (b,c)) err

importWithAction :: ModuleAPI -> ImportAction -> Env -> Errors [ImportError] Env
importWithAction (ModuleAPI p mtr mco mty _ _) (ImportAs p') (Env tr ty co) =
    let rtr = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mtr]
        rty = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mty]
        rco = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mco]
        tr' = checkRenames p tr rtr
        ty' = checkRenames p ty rty
        co' = checkRenames p co rco
    in liftM3 Env tr' ty' co'
importWithAction (ModuleAPI p mtr mco mty _ _) Include (Env tr ty co) =
    let rtr = [(LocalIdentifier n, ExternalIdentifier p n) | n <- fmap fst mtr]
        rty = [(LocalIdentifier n, ExternalIdentifier p n) | n <- fmap fst mty]
        rco = [(LocalIdentifier n, ExternalIdentifier p n) | n <- fmap fst mco]
        tr' = checkRenames p tr rtr
        ty' = checkRenames p ty rty
        co' = checkRenames p co rco
    in liftM3 Env tr' ty' co'

getAPIs :: ModuleServer -> [(ModulePath,ImportAction)] -> Either [ImportError] [(ModuleAPI,ImportAction)]
getAPIs ms [] = pure []
getAPIs ms ((mp,ia):is) = case getAPI ms mp of
    Just api -> ((api,ia):) <$> getAPIs ms is
    Nothing -> Left [NonexistentModule mp]

doImports :: ModuleServer -> [(ModulePath,ImportAction)] -> Errors [ImportError] Env
doImports ms [] = pure emptyEnv
doImports ms ((mp,ia):is) = case getAPI ms mp of
    Just api -> importWithAction api ia =<< doImports ms is
    Nothing -> throw [NonexistentModule mp]