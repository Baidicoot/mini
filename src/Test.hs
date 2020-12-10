module Test where

import Types.CPS
import Types.Prim
import Types.Ident

import CPS.Spill

import Modules.Glue
import Types.Module

import Types.Type
import Data.Set (fromList)

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
            )] [] []

glueTest = glue (LocalIdentifier "__start") 15 ms