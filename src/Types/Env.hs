module Types.Env
    ( ModuleExports(..)
    , GADT(..)
    , includeGADTs
    , ImportAction(..)
    , Env(..)
    , importWithAction
    , include
    , doImports
    , getSignature
    ) where

import Types.Ident
import Types.Syntax
import Types.Type
import Types.Graph

import Control.Arrow
import qualified Data.Map as Map

-- fields include constructor information
data ModuleExports = ModuleExports
    { moduleMod :: ModulePath       -- module path
    , termNames :: [Name]           -- term-level names
    , typeNames :: [Name]           -- type-level names
    , termTypes :: [(Name, Scheme)] -- term-level typing info
    , typeKinds :: [(Name, Kind)]   -- type-level kinding info
    , construct :: [(Name, GADT)]   -- constructor info
    , indGroups :: [(Name, GADT)]   -- type-addressed constructor info
    }
    deriving(Show)

getSignature :: ModuleExports -> Type
getSignature m = Node NoTag (Product $ fmap (\(_,Forall _ t) -> t) (termTypes m))

instance Semigroup ModuleExports where
    (ModuleExports a c e g i k m) <> (ModuleExports _ d f h j l n) = ModuleExports
        a
        (c <> d)
        (e <> f)
        (g <> h)
        (i <> j)
        (k <> l)
        (m <> n)

instance Monoid ModuleExports where
    mempty = ModuleExports mempty mempty mempty mempty mempty mempty mempty
    mappend = (<>)

-- GADT type stores GADTs with their local name.
-- (But global type names)
data GADT
    = GADT Name [(Name,Scheme)]
    deriving(Eq, Show)

infoGADT :: Name -> GADT -> (Int, Scheme, GADT)
infoGADT n g@(GADT _ xs) = internal n xs 0 
    where
        internal n ((o,s):xs) i
            | n == o = (i, s, g)
            | otherwise = internal n xs (i+1)
        internal n _ _ = error ("invalid GADT: " ++ n ++ " in " ++ show g)

includeGADTs :: [GADT] -> ModuleExports
includeGADTs gs = mempty
    { termNames = concatMap (\(GADT _ ns) -> fmap fst ns) gs
    , typeNames = fmap (\(GADT n _) -> n) gs
    , termTypes = concatMap (\(GADT _ ns) -> ns) gs
    , construct = concatMap (\g@(GADT _ ns) -> fmap (\(n,_) -> (n,g)) ns) gs
    , indGroups = fmap (\g@(GADT n _) -> (n, g)) gs
    }

data ImportAction
    = ImportAsHiding ModulePath [Name] [Name]
    deriving(Eq, Show)

data Env = Env 
    { termRenames :: Map.Map Identifier Identifier
    , typeRenames :: Map.Map Identifier Identifier
    , consInfo :: Map.Map Identifier (Int, Scheme, GADT)
    , indInfo :: Map.Map Identifier GADT
    , types :: Map.Map Identifier Scheme
    , kinds :: Map.Map Identifier Kind }
    deriving(Show)

instance Semigroup Env where
    (Env a c e g i k) <> (Env b d f h j l) = Env
        (a <> b)
        (c <> d)
        (e <> f)
        (g <> h)
        (i <> j)
        (k <> l)

instance Monoid Env where
    mempty = Env mempty mempty mempty mempty mempty mempty
    mappend = (<>)

ident :: ModulePath -> Name -> Identifier
ident [] n = LocalIdentifier n
ident xs n = ExternalIdentifier xs n

importAs :: ModulePath -> ModuleExports -> Env
importAs m' (ModuleExports m a b c d e f) = Env
    (Map.fromList $ fmap (ident m' &&& ident m) a)
    (Map.fromList $ fmap (ident m' &&& ident m) b)
    (Map.fromList $ fmap (\(n,g) -> (ident m n, infoGADT n g)) e)
    (Map.fromList $ fmap (first (ident m)) f)
    (Map.fromList $ fmap (first (ident m)) c)
    (Map.fromList $ fmap (first (ident m)) d)

hide :: [Name] -> [Name] -> ModuleExports -> ModuleExports
hide terms types (ModuleExports m a b c d e f) = ModuleExports m
    (filter (not . (`elem` terms)) a)
    (filter (not . (`elem` types)) b)
    (filter (not . (`elem` terms) . fst) c)
    (filter (not . (`elem` types) . fst) d)
    (filter (not . (`elem` terms) . fst) e)
    (filter (not . (`elem` types) . fst) f)

include :: ImportAction
include = ImportAsHiding [] [] []

importWithAction :: ModuleExports -> ImportAction -> Env
importWithAction me (ImportAsHiding m ns ts) = importAs [] (hide ns ts me)

doImports :: [(ModuleExports,ImportAction)] -> Env
doImports = mconcat . fmap (uncurry importWithAction)