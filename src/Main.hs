module Main where

import Build.Build
import Types.Build

import Backend.Interpreter
import Backend.C

import Control.Monad.Errors
import System.Environment

import qualified Test as T

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

main :: IO ()
main = do
    (mode:root:args) <- getArgs
    case mode of
        "cps" -> T.main root args
        _ -> do
            let cfg = case mode of
                    "c" -> BuildConfig root cbackend "c" []
                    "abst" -> BuildConfig root (interpreter 20) "interpret" []
            r <- runErrorsT $ build cfg (fmap (wordsWhen (=='.')) args)
            case toEither r of
                Left e -> mapM_ putStrLn e
                Right _ -> putStr "\n"