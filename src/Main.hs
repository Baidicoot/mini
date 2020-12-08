module Main where

import Build.Build
import Types.Build

import Backend.X86_64_linux.Textual

import Control.Monad.Errors
import System.Environment

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

main :: IO ()
main = do
    args <- fmap (wordsWhen (=='.')) <$> getArgs
    r <- runErrorsT $ build (BuildConfig "./" x86_64_linuxTextualBackend "x86_64-linux-textual" []) args
    case toEither r of
        Left e -> mapM_ putStrLn e
        Right e -> putStrLn ("main file is: " ++ e)
