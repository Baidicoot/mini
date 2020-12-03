module Modules.CoreToAbst where

import Types.Abstract
import Types.Core

import CPS.CPSify
import CPS.ClosureConv

coreToAbst :: [Identifier] -> Env -> Int -> Core Type -> Int -> ([Operand], Int)
coreToAbst k e r c0 s0 =
    let (c1,(s1,_)) = cpsify k e (untagCore c0) s0
        (c2,s2) = closureConv c1 s1
        (c3,s3) = spill r c2 s2
    in undefined