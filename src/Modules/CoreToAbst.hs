module Modules.CoreToAbst where

import Types.Abstract
import Types.Core
import Types.Ident
import Types.Module
import Types.Type

import CPS.CPSify
import CPS.ClosureConv
import CPS.Spill
import Backend.AbstGen

coreToAbst :: [Identifier] -> Identifier -> ModuleServer -> Int -> Core Type -> Int -> ([Operator], Int)
coreToAbst k m e r c0 s0 =
    let (c1,(s1,_)) = cpsify k e (untagCore c0) s0
        (c2,s2) = closureConv c1 s1
        (c3,s3) = spill r s2 c2
        ops = generateAbstract (filter ((`elem` k) . fst) (regLayouts e)) m c3 r
    in (ops,s3)