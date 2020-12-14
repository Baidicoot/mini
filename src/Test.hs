module Test where

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

compile :: Int -> Int -> Identifier -> ModuleServer -> [(ModulePath,Either CachedFile (ParseResult,Stream))] -> [(ModulePath,CExp)] -> Build ()
compile index num mainLabel ms ((p,Right (pr,s)):fs) done = do
    liftIO . putStrLn $ "compiling " ++ intercalate "." p ++ "... (" ++ show index ++ " of " ++ show num ++ ")"
    (w,api,abi,ops,(ucc,t)) <- liftEither $ parsedToCPS p ms [] (mainFn p) s pr
    -- liftIO $ prettyPrint ops (0::Int)
    -- liftIO . forM_ (Map.toList (getCaptured ucc)) $ \(i,b) -> putStrLn (show i ++ ":" ++ concatMap ((' ':) . show) (Set.toList b))
    -- liftIO . forM_ (Map.toList (getBound ucc)) $ \(i,b) -> putStrLn (show i ++ ":" ++ concatMap ((' ':) . show) (Set.toList b))
    -- liftIO $ prettyPrint t (0::Int)
    liftIO $ mapM_ putStrLn w
    compile (index+1) num mainLabel (loadModule abi api ms) fs ((p,ops):done)
compile _ _ mainLabel ms [] done = do
    g <- liftEither . mapLeft (fmap show) $ glueToCPS mainLabel ms
    -- liftIO $ print g
    -- liftIO $ putStr "\n"
    liftIO $ interpret mainLabel (([],g):done)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

build :: String -> [ModulePath] -> Build ()
build root paths = do
    fs <- load root paths
    compile 0 (length paths) (LocalIdentifier "start") emptyServer fs []

main :: String -> [String] -> IO ()
main root args = do
    x <- fmap toEither . runErrorsT $ build root (fmap (wordsWhen (=='.')) args)
    case x of
        Left e -> mapM_ putStrLn e
        Right _ -> putStr "\n"