{-# LANGUAGE LambdaCase #-}
module Frontend.GenEnv where

import Types.Ident
import Types.Syntax
import Types.Type
import qualified Data.Map as Map

data Namespace
    = Namespace [Name] [Name] [Name]
    deriving(Eq, Show)

data Typespace
    = Typespace (Map.Map Identifier Scheme) (Map.Map Identifier Scheme)
    deriving(Eq, Show)

data Dataspace
    = Dataspace (Map.Map Identifier (Int, Int, Int))
    -- for every constructor: index, number of constructors, number of arguments
    deriving(Eq, Show)

data ImportAction
    = Include Namespace
    | IncludeHiding Namespace [Name]
    | Import Namespace
    | ImportAs Namespace [Name]
    deriving(Eq, Show)

genImportMap :: ImportAction -> (Map.Map Identifier Identifier, Map.Map Identifier Identifier)
genImportMap (Include (Namespace start ns ts)) =
    let a = Map.fromList (fmap (\n -> (LocalIdentifier n, ExternalIdentifier start n)) ns)
        b = Map.fromList (fmap (\n -> (LocalIdentifier n, ExternalIdentifier start n)) ts) in
            (a, b)
genImportMap (ImportAs (Namespace start ns ts) syn) =
    let a = Map.fromList (fmap (\n -> (ExternalIdentifier syn n, ExternalIdentifier start n)) ns)
        b = Map.fromList (fmap (\n -> (ExternalIdentifier syn n, ExternalIdentifier start n)) ts) in
            (a, b)
genImportMap (IncludeHiding (Namespace start ns ts) without) = genImportMap (Include (Namespace start (filter (not . flip elem without) ns) (filter (not . flip elem without) ts)))
genImportMap (Import (Namespace start ns ts)) = genImportMap (ImportAs (Namespace start ns ts) start)

genNamespace :: [Name] -> [TopLevel] -> Namespace
genNamespace ns ts = Namespace ns (collectNames ts) (collectTypeNames ts)
    where
        collectTopLevel :: TopLevel -> [Name]
        collectTopLevel (Func (Defn _ n _ _)) = [n]
        collectTopLevel (Data (Ind _ _ as)) = fmap (\(Expl n _) -> n) as

        collectNames :: [TopLevel] -> [Name]
        collectNames = concatMap (\t -> collectTopLevel t)

        collectTypeNames :: [TopLevel] -> [Name]
        collectTypeNames = concatMap (\case
            Func _ -> []
            (Data (Ind n _ _)) -> [n])

genDataspace :: [Name] -> [TopLevel] -> Dataspace
genDataspace ns ts = Dataspace (collectDatas ts)
    where
        collectData :: TopLevel -> Map.Map Identifier (Int, Int, Int)
        collectData (Data (Ind _ _ cs)) = Map.fromList . fmap (\(Expl n t, i) -> (ExternalIdentifier ns n, (i, length cs, arity t))) $ zip cs [0..]
        collectData _ = mempty

        collectDatas :: [TopLevel] -> Map.Map Identifier (Int, Int, Int)
        collectDatas = mconcat . fmap collectData