module Main where

import Build.Build
import Types.Build

import Backend.Interpreter
import Backend.C

import Control.Monad.Errors
import Control.Monad
import System.Environment
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath

import qualified CPSMachine as M

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)

traverseDir :: (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir transition state dirPath = do
    names <- listDirectory dirPath
    let paths = map (dirPath </>) names
    (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
    state' <- foldM transition state filePaths
    foldM (traverseDir transition) state' dirPaths

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

parsePaths :: [String] -> [[String]]
parsePaths (('-':_):xs) = parsePaths xs
parsePaths (x:xs) = wordsWhen (=='.') x:parsePaths xs
parsePaths [] = []

isMiniExt :: String -> Bool
isMiniExt s = drop (length s - 3) s == ".mi"

getMiniSourceFiles :: FilePath -> IO [[String]]
getMiniSourceFiles = traverseDir (\xs x ->
    if isMiniExt x then
        pure (tail (wordsWhen (=='/') (take (length x - 3) x)):xs)
    else pure xs) []

main :: IO ()
main = do
    (mode:root:args) <- getArgs
    --paths <- getMiniSourceFiles root
    --print paths
    let paths = parsePaths args
    case mode of
        "cps" -> M.main root paths args
        "c" -> do
            r <- runErrorsT $ build (BuildConfig root cbackend "c" args) paths
            case toEither r of
                Left e -> mapM_ putStrLn e
                Right _ -> putStr "\n"
        "abst" -> do
            r <- runErrorsT $ build (BuildConfig root (interpreter 20) "interpret" args) paths
            case toEither r of
                Left e -> mapM_ putStrLn e
                Right _ -> putStr "\n"
        _ -> putStrLn "please specify a mode-of-operation"