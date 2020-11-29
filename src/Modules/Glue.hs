module Modules.Glue where

import Types.Ident
import Types.Abstract
import Types.Type
import Types.Core
import Types.Graph
import Types.Prim

import Backend.AbstGen
import Modules.Server
import Data.List

data ImportError
    = CircularDependency ModuleServer
    deriving(Show)

sortModules :: ModuleServer -> Either ImportError [Module]
sortModules = internal []
    where
        internal :: [Module] -> ModuleServer -> Either ImportError [Module]
        internal sorted [] = Right sorted
        internal sorted remaining =
            let (solved, left) = partition (satisfiedDependencies sorted) remaining
            in case solved of
                [] -> Left (CircularDependency (sorted ++ remaining))
                _ -> internal (sorted ++ solved) left

        satisfiedDependencies :: [Module] -> Module -> Bool
        satisfiedDependencies has m = null (moduleReqs m \\ fmap modulePath has)

modName :: ModulePath -> Identifier
modName m = ExternalIdentifier m "mod"

glueLExp :: [Module] -> [(ModulePath, Type)] -> Core Type
glueLExp [] _ = Node unitty . Val $ Lit Unit
glueLExp (m:ms) loaded = Node unitty $ Let name resTerm (glueLExp ms ((modulePath m,resTyp):loaded))
    where
        name :: Identifier
        name = modName (modulePath m)

        argTyp :: Type
        argTyp = Node NoTag . Product . fmap ((\(Just x) -> x) . (`lookup` loaded)) $ moduleReqs m

        argTerm :: Core Type
        argTerm = Node argTyp . Tuple . fmap (Var . modName) $ moduleReqs m

        resTyp :: Type
        resTyp = moduleSign m

        resTerm :: Core Type
        resTerm = App resTyp (Node (argTyp --> resTyp) . Val $ Var name) argTerm

glue :: ModuleServer -> [Operand]
glue ms = undefined