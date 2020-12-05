module Modules.TextToCore (textToCore) where

import Text.Parsec
import Parser.Parser
import Parser.SExpr

import Elaborate.Elaborate
import TypeCheck.Check
import Modules.Defunctorize

import Types.Core
import Types.Ident
import Types.Type
import Types.Module

import Data.List

import Error.Error
import Control.Monad.Errors

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

textToCore :: ModulePath -> ModuleServer -> String -> Int -> Either [String] ([String], ModuleAPI, Core Type, Int)
textToCore p ser s s0 = do
    toks <- mapLeft ((:[]) . show) $ parse (many rpncc) (intercalate "." p) s
    (imports,parsed) <- mapLeft (fmap (render s)) $ program toks
    env <- mapLeft (fmap show) . toEither . runErrors $ doImports ser imports
    (untyped,gadts,exports,s1,w0) <- mapLeft
        (\(as, bs) -> fmap (render s) as ++ fmap (render s) bs)
        . toEither $ elaborate s0 ser env p parsed
    let ser' = let (ModuleServer abis apis gadts') = ser in ModuleServer abis apis (gadts ++ gadts')
    (typed, types, s2) <- mapLeft (fmap (render s)) . toEither $ typecheck s1 ser' untyped
    let (s3, defunc) = defunctorize (mainFn p) [] s2 typed
    let exportSchemes = zip exports types
    pure (fmap (render s) w0, constructAPI p exportSchemes gadts, defunc, s3)