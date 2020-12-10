module Main where

import Build.Build
import Types.Build

import Backend.C

import Control.Monad.Errors
import System.Environment

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

main :: IO ()
main = do
    (root:args) <- getArgs
    r <- runErrorsT $ build (BuildConfig root cbackend "x86_64-linux-textual" []) (fmap (wordsWhen (=='.')) args)
    case toEither r of
        Left e -> mapM_ putStrLn e
        Right e -> putStrLn ("main file is: " ++ e)
