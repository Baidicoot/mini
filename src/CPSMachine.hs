module CPSMachine where

import Modules.Glue
import Types.Build
import Types.Ident
import Types.Module
import Parser.Parser
import Types.CPS
import Modules.Module
import Data.List
import Control.Monad.IO.Class
import Control.Monad.Errors
import Types.Pretty
import CPS.Interpreter
import CPS.Meta
import Build.Load
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

compile :: Int -> Int -> Identifier -> ModuleServer -> [(ModulePath,Either CachedFile (ParseResult,Stream))] -> [(ModulePath,CExp)] -> Build [(ModulePath,CExp)]
compile index num mainLabel ms ((p,Right (pr,s)):fs) done = do
    liftIO . putStrLn $ "\ncompiling " ++ intercalate "." p ++ "... (" ++ show index ++ " of " ++ show num ++ ")"
    (w,api,abi,ops,(ucc,t)) <- liftEither $ parsedToCPS p ms [] (mainFn p) s pr
    liftIO $ mapM_ putStrLn w
    liftIO . forM_ (moduleAPITerms api) $ \(n,s) -> putStrLn ("defined " ++ n ++ " : " ++ show s)
    compile (index+1) num mainLabel (loadModule abi api ms) fs ((p,ops):done)
compile _ _ mainLabel ms [] done = do
    g <- liftEither . mapLeft (fmap show) $ glueToCPS mainLabel ms
    liftIO $ putStr "\n"
    pure (([],g):done)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

build :: String -> [ModulePath] -> Build [(ModulePath,CExp)]
build root paths = do
    fs <- load root paths
    compile 1 (length paths) (LocalIdentifier "start") emptyServer fs []

mainBuild :: String -> [[String]] -> [String] -> Build ()
mainBuild root paths args = do
    fs <- build root paths
    if "--dump" `elem` args then
        liftIO $ mapM_ (\(n,p) -> print n >> prettyPrint p (0::Int)) fs
    else pure ()
    liftIO $ interpret (LocalIdentifier "start") fs
    pure ()

main :: String -> [[String]] -> [String] -> IO ()
main root paths args = do
    x <- fmap toEither . runErrorsT $ mainBuild root paths args
    case x of
        Left e -> mapM_ putStrLn e
        Right _ -> putStr "\n"