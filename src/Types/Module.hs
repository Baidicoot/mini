module Types.Module where

import Types.Ident
import Types.Type
import Types.Graph
import Types.Abstract

import Data.List

import Control.Arrow
import Control.Monad

data ModuleABI = ModuleABI
    { moduleABIPath :: ModulePath
    , moduleABIMain :: Name
    , moduleABIReqs :: [ModulePath]
    , moduleABIRegs :: [GPR]
    }

data ModuleAPI = ModuleAPI
    { moduleAPIPath :: ModulePath
    , moduleAPITerms :: [(Name, Scheme)]
    , moduleAPITypes :: [(Name, Kind)]
    , moduleAPIGADTs :: [GADT]
    }

data GADT = GADT
    { gadtName :: Identifier
    , gadtKind :: Kind
    , gadtCons :: [(Identifier, Scheme)]
    }

data ModuleServer = ModuleServer
    { abis :: [ModuleABI]
    , apis :: [ModuleAPI]
    , gadts :: [GADT]
    }

emptyServer :: ModuleServer
emptyServer = ModuleServer mempty mempty mempty

termTypes :: ModuleServer -> [(Identifier, Scheme)]
termTypes (ModuleServer _ apis _) = concatMap (\(ModuleAPI p t _ _) -> fmap (first (ExternalIdentifier p)) t) apis

typeKinds :: ModuleServer -> [(Identifier, Kind)]
typeKinds (ModuleServer _ apis _) = concatMap (\(ModuleAPI p _ t _) -> fmap (first (ExternalIdentifier p)) t) apis

consTypes :: ModuleServer -> [(Identifier,Scheme)]
consTypes = concatMap gadtCons . gadts

regLayouts :: ModuleServer -> [(Identifier,[GPR])]
regLayouts = fmap (\(ModuleABI p n _ l) -> (ExternalIdentifier p n, l)) . abis

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
    = NameCollision Identifier (Identifier, Identifier)
    | UnfulfilledDependency [ModulePath]

checkRenames :: [(Identifier, Identifier)] -> [(Identifier, Identifier)] -> Either [ImportError] [(Identifier, Identifier)]
checkRenames p p' = case partition ((`elem` fmap fst p) . fst) p' of
    ([], xs) -> Right (p ++ xs)
    (err,_) -> Left $ fmap (\(a,b) -> let (Just c) = lookup a p in NameCollision a (b,c)) err

importWithAction :: Env -> ModuleAPI -> ImportAction -> Either [ImportError] Env
importWithAction (Env tr ty co) (ModuleAPI p mtr mty gs) (ImportAs p') =
    let rtr = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mtr]
        rty = [(ExternalIdentifier p' n, ExternalIdentifier p n) | n <- fmap fst mty]
        rco = [n | GADT _ _ co <- gs, n <- fmap fst co]
        tr' = checkRenames tr rtr
        ty' = checkRenames ty rty
        co' = Right $ nub (rco ++ co)
    in liftM3 Env tr' ty' co'