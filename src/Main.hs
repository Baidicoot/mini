module Main where

import Build.Build
import Types.Build

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
    (root:args) <- getArgs
    T.main root args
