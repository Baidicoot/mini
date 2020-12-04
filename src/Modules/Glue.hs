module Modules.Glue where

import Types.Ident
import Types.Abstract
import Types.Type
import Types.Module
import Types.Core
import Types.Graph
import Types.Prim

import Modules.CoreToAbst
import Data.List

sortModules :: ModuleServer -> Either [ImportError] [ModuleABI]
sortModules = internal [] . abis
    where
        internal :: [ModuleABI] -> [ModuleABI] -> Either [ImportError] [ModuleABI]
        internal sorted [] = Right sorted
        internal sorted remaining =
            let (solved, left) = partition (satisfiedDependencies sorted) remaining
            in case solved of
                [] -> Left [UnfulfilledDependency . fmap moduleABIPath $ sorted ++ remaining]
                _ -> internal (sorted ++ solved) left

        satisfiedDependencies :: [ModuleABI] -> ModuleABI -> Bool
        satisfiedDependencies has m = null (moduleABIReqs m \\ fmap moduleABIPath has)

modName :: ModulePath -> Identifier
modName m = ExternalIdentifier m "mod"

glueLExp :: ModuleServer -> [ModuleABI] -> [(ModulePath, Type)] -> Core Type
glueLExp s [] _ = Node unitty . Val $ Lit Unit
glueLExp s (m:ms) loaded = Node unitty $ Let name resTerm (glueLExp s ms ((moduleABIPath m,resTyp):loaded))
    where
        name :: Identifier
        name = modName (moduleABIPath m)

        argTyp :: Type
        argTyp = Node NoTag . Product . fmap ((\(Just x) -> x) . (`lookup` loaded)) $ moduleABIReqs m

        argTerm :: Core Type
        argTerm = Node argTyp . Tuple . fmap (Var . modName) $ moduleABIReqs m

        resTyp :: Type
        resTyp = (\(Just api) -> getSignature api) (getAPI s (moduleABIPath m))

        resTerm :: Core Type
        resTerm = App resTyp (Node (argTyp --> resTyp) . Val $ Var name) argTerm

glue :: ModuleServer -> Either [ImportError] [Operand]
glue ms = do
    abis <- sortModules ms
    pure (glueLExp ms abis [])