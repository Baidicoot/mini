module Glue where
-- Glue: hiding things from Main

import Text.Parsec
import Parser.SExpr
import Modules.TextToCore (textToCore)
import Modules.CoreToAbst

import Control.Monad
import Control.Monad.Except

import Types.Module
import Types.Pretty

import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

type Repl = ExceptT [String] IO
newtype Config = Config {regs :: Int}

handleEither :: Either a b -> (a -> Repl b) -> Repl b
handleEither (Right b) f = pure b
handleEither (Left a) f = f a

compileStr :: Config -> String -> Repl ModuleAPI
compileStr config s = do
    let path = ["Repl"]
    (w,api,c,s0) <- textToCore path emptyServer s 0 `handleEither` throwError
    liftIO $ mapM putStrLn w
    liftIO $ prettyPrint c (0::Int)
    let (abst,s1) = coreToAbst [] [mainFn path] emptyServer (regs config) c s0
    liftIO $ putStrLn ""
    liftIO $ print abst
    liftIO $ mapM (\(i,s) -> putStrLn $ "defined `" ++ i ++ "`: " ++ show s) (moduleAPITerms api)
    pure api