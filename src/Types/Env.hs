module Types.Env
    ( ModuleExports(..)
    , GADT(..)
    , includeGADTs
    , ImportAction(..)
    , Env(..)
    , importWithAction
    ) where

import Types.Ident
import Types.Syntax
import Types.Type

import Control.Arrow
import qualified Data.Map as Map

import Data.Semigroup

-- fields include constructor information
data ModuleExports = ModuleExports
    { moduleMod :: Module           -- module path
    , termNames :: [Name]           -- term-level names
    , typeNames :: [Name]           -- type-level names
    , termTypes :: [(Name, Scheme)] -- term-level typing info
    , typeKinds :: [(Name, Kind)]   -- type-level kinding info
    , construct :: [(Name, GADT)]   -- constructor info
    }

instance Monoid ModuleExports where
    mempty = ModuleExports mempty mempty mempty mempty mempty mempty
    mappend (ModuleExports a c e g i k) (ModuleExports _ d f h j l) = ModuleExports
        a
        (c <> d)
        (e <> f)
        (g <> h)
        (i <> j)
        (k <> l)

-- GADT type stores GADTs with their local name.
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

includeGADTs :: Module -> [GADT] -> ModuleExports
includeGADTs m gs = mempty
    { moduleMod = m
    , termNames = concatMap (\(GADT _ ns) -> fmap fst ns) gs
    , typeNames = fmap (\(GADT n _) -> n) gs
    , termTypes = concatMap (\(GADT _ ns) -> ns) gs
    , construct = concatMap (\g@(GADT _ ns) -> fmap (\(n,_) -> (n,g)) ns) gs
    }

data ImportAction
    = ImportAsHiding Module [Name] [Name]
    deriving(Eq, Show)

data Env = Env 
    { termRenames :: Map.Map Identifier Identifier
    , typeRenames :: Map.Map Identifier Identifier
    , consInfo :: Map.Map Identifier (Int, Scheme, GADT)
    , types :: Map.Map Identifier Scheme
    , kinds :: Map.Map Identifier Kind }

instance Monoid Env where
    mempty = Env mempty mempty mempty mempty mempty
    mappend (Env a c e g i) (Env b d f h j) = Env (a <> b) (c <> d) (e <> f) (g <> h) (i <> j)

ident :: Module -> Name -> Identifier
ident [] n = LocalIdentifier n
ident xs n = ExternalIdentifier xs n

importAs :: Module -> ModuleExports -> Env
importAs m' (ModuleExports m a b c d e) = Env
    (Map.fromList $ fmap (ident m' &&& ident m) a)
    (Map.fromList $ fmap (ident m' &&& ident m) b)
    (Map.fromList $ fmap (\(n,g) -> (ident m n, infoGADT n g)) e)
    (Map.fromList $ fmap (first (ident m)) c)
    (Map.fromList $ fmap (first (ident m)) d)

hide :: [Name] -> [Name] -> ModuleExports -> ModuleExports
hide terms types (ModuleExports m a b c d e) = ModuleExports m
    (filter (not . (`elem` terms)) a)
    (filter (not . (`elem` types)) b)
    (filter (not . (`elem` terms) . fst) c)
    (filter (not . (`elem` types) . fst) d)
    (filter (not . (`elem` terms) . fst) e)

importWithAction :: ImportAction -> ModuleExports -> Env
importWithAction (ImportAsHiding m ns ts) me = importAs [] (hide ns ts me)