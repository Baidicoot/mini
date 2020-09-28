module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Type
import Types.Ident
import Types.Graph
import Types.Pretty
import Types.IR

import Control.Monad
import qualified Data.Map as Map

import Frontend.IRify
import Frontend.GenEnv
import Frontend.Constraint
import Frontend.Solve
import Frontend.CPSify
import Frontend.ClosureConv
import Frontend.Spill

import qualified Types.CPS as CPS

import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main = forever $ do
    line <- prompt "> "
    case parse (many rpncc) "" line of
        Left err -> print err
        Right a -> case runParse (parsetoplevel a) of
            Left err -> print err
            Right b ->
                let ds = genDataspace b
                    ns = genNamespace b
                    in case irify ds ns b of
                        Left err -> print err
                        Right c -> print c