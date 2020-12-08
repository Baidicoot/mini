module Modules.Glue where

import Types.Ident
import Types.Abstract
import Types.Type
import Types.Module
import Types.Core
import Types.Graph
import Types.Prim

import Modules.Module
import Data.List
import Control.Monad.Errors

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

glue :: Identifier -> Int -> ModuleServer -> Either [ImportError] [Operator]
glue main regs ms = do
    abis <- fmap snd <$> sortDependencies (fmap (\x -> (moduleABIPath x,moduleABIReqs x,x)) (abis ms))
    let core = glueLExp ms abis []
    let ops = (\(x,_,_)->x) $ coreToAbst (fmap (mainFn . moduleABIPath) abis) [] ms 0 core regs
    pure (Exports main:Define main:ops)