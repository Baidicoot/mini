module Modules.Glue where

import Types.Ident
import Types.Abstract
import Types.Type
import Types.Module
import Types.Core
import Types.Graph
import Types.Prim
import Types.Pretty
import qualified Types.CPS as CPS

import CPS.CPSify
import CPS.ClosureConv
import CPS.Spill
import Backend.AbstGen

import Modules.Module
import Data.List
import Control.Monad.Errors

modName :: ModulePath -> Identifier
modName m = ExternalIdentifier m "mod"

glueLExp :: ModuleServer -> [ModuleABI] -> [(ModulePath, Type)] -> Core Type
glueLExp s [] _ = Node unitty $ Tuple []
glueLExp s (m:ms) loaded = Node unitty $ Let name resTerm (glueLExp s ms ((moduleABIPath m,resTyp):loaded))
    where
        name :: Identifier
        name = modName (moduleABIPath m)

        main :: Identifier
        main = mainFn (moduleABIPath m)

        argTyp :: Type
        argTyp = Node NoTag . Product . fmap ((\(Just x) -> x) . (`lookup` loaded)) $ moduleABIReqs m

        argTerm :: Core Type
        argTerm = Node argTyp . Tuple . fmap (Var . modName) $ moduleABIReqs m

        resTyp :: Type
        resTyp = (\(Just api) -> getSignature api) (getAPI s (moduleABIPath m))

        resTerm :: Core Type
        resTerm = App resTyp (Node (argTyp --> resTyp) . Val $ Var main) argTerm

glueCoreToAbst :: [Identifier] -> Identifier -> ModuleServer -> Int -> Core Type -> Int -> ([Operator], [(Identifier, [GPR])], Int)
glueCoreToAbst k m e r c0 s0 =
    let (c1,(s1,_)) = cpsify k e (untagCore c0) s0
        (CPS.Fix defs exp,s2) = closureConv c1 s1
        c2 = CPS.Fix (CPS.Fun m [] exp:defs) CPS.Halt
        (c3,s3) = spill r s2 c2
        (l,ops) = generateAbstract (filter ((`elem` k) . fst) (regLayouts e)) [m] c3 r
    in (ops,l,s3)

glue :: Identifier -> Int -> ModuleServer -> Either [ImportError] [Operator]
glue main regs ms = do
    abis <- fmap snd <$> sortDependencies (fmap (\x -> (moduleABIPath x,moduleABIReqs x,x)) (abis ms))
    let core = glueLExp ms abis []
    let ops = (\(x,_,_)->x) $ glueCoreToAbst (fmap (mainFn . moduleABIPath) abis) main ms regs core 0
    pure ops