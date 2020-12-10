module Modules.Module (parsedToCore, coreToAbst, parsedToAbst) where

import Parser.Parser

import Elaborate.Elaborate
import TypeCheck.Check
import Modules.Defunctorize
import CPS.CPSify
import CPS.ClosureConv
import CPS.Spill
import Backend.AbstGen

import Types.Core
import Types.Ident
import Types.Type
import Types.Module
import Types.Abstract

import Data.List

import Error.Error
import Control.Monad.Errors

parsedToCore :: ModulePath -> ModuleServer -> Int -> Stream -> ParseResult -> Either [String] ([String], ModuleAPI, Core Type, Int)
parsedToCore p ser s0 s (imports,parsed) = do
    env <- mapLeft (fmap show) . toEither . runErrors $ doImports ser imports
    (untyped,gadts,exports,s1,w0) <- mapLeft
        (\(as, bs) -> fmap (render s) as ++ fmap (render s) bs)
        . toEither $ elaborate s0 ser env p parsed
    let ser' = let (ModuleServer abis apis gadts') = ser in ModuleServer abis apis (gadts ++ gadts')
    (typed, types, s2) <- mapLeft (fmap (render s)) . toEither $ typecheck s1 ser' untyped
    let (s3, defunc) = defunctorize (mainFn p) [] s2 typed
    let exportSchemes = zip exports types
    pure (fmap (render s) w0, constructAPI p exportSchemes gadts, defunc, s3)

coreToAbst :: [Identifier] -> [Identifier] -> ModuleServer -> Int -> Core Type -> Int -> ([Operator], [(Identifier,[GPR])], Int)
coreToAbst k m e r c0 s0 =
    let (c1,(s1,_)) = cpsify k e (untagCore c0) s0
        (c2,s2) = closureConv c1 s1
        (c3,s3) = spill r s2 c2
        (l,ops) = generateAbstract (filter ((`elem` k) . fst) (regLayouts e)) m c3 r
    in (ops,l,s3)

parsedToAbst :: ModulePath -> ModuleServer -> [Identifier] -> Identifier -> Int -> Stream -> ParseResult -> Either [String] ([String], ModuleAPI, ModuleABI, [Operator])
parsedToAbst p ms k m r s pr@(i,_) = do
    (w,a,t,s0) <- parsedToCore p ms 0 s pr
    let (o,[(_,l)],_) = coreToAbst k [m] (loadAPI a ms) r t s0
    pure (w,a,ModuleABI p m (fmap fst i) l,o)