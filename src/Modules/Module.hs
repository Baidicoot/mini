module Modules.Module (parsedToCore, coreToAbst, parsedToAbst, coreToCPS, parsedToCPS) where

import Parser.Parser

import Elaborate.Elaborate
import TypeCheck.Check
import Modules.Defunctorize
import CPS.CPSify
import CPS.ClosureConv
import CPS.CPSOpt
import CPS.Spill
import CPS.Meta
import Backend.AbstGen

import Types.Core
import qualified Types.CPS as CPS
import qualified Data.Map as Map
import Types.Ident
import Types.Type
import Types.Module
import Types.Abstract
import Types.Build

import Data.List

import Error.Error
import Control.Monad.Errors

import Types.Pretty

parsedToCore :: ModulePath -> ModuleServer -> Int -> Stream -> ParseResult -> Either [String] ([String], ModuleAPI, Core Type, Int)
parsedToCore p ser s0 s (imports,parsed) = do
    env <- mapLeft (fmap show) . toEither . runErrors $ doImports ser imports
    (untyped,gadts,eqtns,exports,s1,w0) <- mapLeft
        (\(as, bs) -> fmap (render s) as ++ fmap (render s) bs)
        . toEither $ elaborate s0 ser env p parsed
    let ser' = let (ModuleServer abis apis gadts' eqtns') = ser in ModuleServer abis apis (gadts ++ gadts') (eqtns ++ eqtns')
    (typed, types, s2) <- mapLeft (fmap (render s)) . toEither $ typecheck s1 ser' untyped
    imports <- mapLeft (fmap show) (getAPIs ser imports)
    let (s3, defunc) = defunctorize (mainFn p) (fmap fst imports) s2 typed
    let exportSchemes = zip exports types
    pure (fmap (render s) w0, constructAPI p exportSchemes gadts eqtns, defunc, s3)

coreToAbst :: OptFlags -> [Identifier] -> [Identifier] -> ModuleServer -> Int -> Core Type -> Int -> ([Operator], [(Identifier,[GPR])], Int)
coreToAbst f k m e r c0 s0 =
    let (c1,(s1,_)) = cpsify k e m (untagCore c0) s0
        (c2,s2) = closureConv c1 s1
        (c3,s3) = cpsOpt s2 f c2
        (c4,s4) = spill r s3 c3
        (l,ops) = generateAbstract (filter ((`elem` k) . fst) (regLayouts e)) m c4 r
    in (ops,l,s4)

coreToCPS :: OptFlags -> [Identifier] -> [Identifier] -> ModuleServer -> Core Type -> Int -> (CPS.CExp,Int,CPS.CExp)
coreToCPS f k m e c0 s0 =
    let (c1,(s1,_)) = cpsify k e m (untagCore c0) s0
        (c2,s2) = closureConv c1 s1
        (c3,s3) = cpsOpt s2 f c2
    in (c3,s3,c2)

parsedToAbst :: OptFlags -> ModulePath -> ModuleServer -> [Identifier] -> Identifier -> Int -> Stream -> ParseResult -> Either [String] ([String], ModuleAPI, ModuleABI, [Operator], Core Type)
parsedToAbst f p ms k m r s pr@(i,_) = do
    (w,a,t,s0) <- parsedToCore p ms 0 s pr
    let (o,[(_,l)],_) = coreToAbst f k [m] (loadAPI a ms) r t s0
    pure (w,a,ModuleABI p m (fmap fst i) l,o, t)

parsedToCPS :: OptFlags -> ModulePath -> ModuleServer -> [Identifier] -> Identifier -> Stream -> ParseResult -> Either [String] ([String], ModuleAPI, ModuleABI, CPS.CExp, (CPS.CExp,Core Type))
parsedToCPS f p ms k m s pr@(i,_) = do
    (w,a,t,s0) <- parsedToCore p ms 0 s pr
    let (c,_,unopt) = coreToCPS f k [m] (loadAPI a ms) t s0
    pure (w,a,ModuleABI p m (fmap fst i) [],c,(unopt,t))