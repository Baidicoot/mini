module Test where

import Types.CPS
import Types.Prim
import Types.Ident

import CPS.Spill

import Modules.Glue
import Types.Module

import Types.Type
import Data.Set (fromList)

import qualified Types.Syntax as Syn
import Elaborate.Elaborate
import Elaborate.Elaborator

import qualified Types.Core as Core
import Text.Parsec.Pos

import qualified Types.Graph as Graph

import Control.Monad.Errors

testPos :: SourcePos
testPos = initialPos "hjahaha"

fixTest :: ErrorsResult ([ElabError], [ElabWarning]) (Core.Core SourcePos, [GADT], [Name], Int, [ElabWarning])
fixTest = elaborate 0 emptyServer emptyEnv [] [tl] 
    where
        tl = Syn.Group testPos [f]
        f = Syn.FunDef testPos Nothing "f" ["x"] (Graph.App testPos
            (Graph.Node testPos $ Syn.Var (LocalIdentifier "f"))
            (Graph.Node testPos $ Syn.Var (LocalIdentifier "x")))

spillTest :: CExp
spillTest = fst $ spill 15 20 testExp
    where
        testExp = Fix [
            __start,
            k16,
            split_k16,
            k15] Halt
        
        __start = Fun (LocalIdentifier "__start") []
            $ Record [(Label (LocalIdentifier "split_k16"),NoPath)] (LocalIdentifier "c20")
            $ App (Label (ExternalIdentifier ["Comb"] "main")) [Lit (Int 0), Var (LocalIdentifier "c20")]
        
        k16 = Fun (LocalIdentifier "k16") [LocalIdentifier "v17"]
            $ App (Label (LocalIdentifier "k15")) [Var (LocalIdentifier "v17")]
        
        split_k16 = Fun (LocalIdentifier "split_k16") [LocalIdentifier "c19", LocalIdentifier "c20"]
            $ App (Label (LocalIdentifier "k16")) [Var (LocalIdentifier "c19")]
        
        k15 = Fun (LocalIdentifier "k15") [ExternalIdentifier ["Comb"] "mod"] Halt

ms = ModuleServer [abi] [api] []
    where
        abi = ModuleABI ["Comb"] (mainFn ["Comb"]) [] [2,1]

        api = ModuleAPI ["Comb"] [("s",Forall (fromList ["x","y","z"])
            $ (tv "x" --> tv "y" --> tv "z") --> (tv "x" --> tv "y") --> tv "x" --> tv "z"
            )] [] [] []

glueTest = glue (LocalIdentifier "__start") 15 ms