module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Type
import Types.Ident
import Types.Graph
import Types.Pretty
import Types.IR
import Types.Env

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

data Config = Config {regs :: Int}

config :: Config
config = Config {regs=10}

main = forever $ do
    line <- prompt "> "
    case parse (many rpncc) "" line of
        Left err -> print err
        Right a -> case runParse (parsetoplevel a) of
            Left err -> print err
            Right b ->
                let ds = genDataspace ["Repl"] b
                    ns = genNamespace ["Repl"] b
                    in case irify ds ns b of
                        Left err -> print err
                        Right c -> do
                            putStrLn "Untyped:"
                            prettyPrint c (0::Int)
                            case annotate (Typespace mempty mempty) c of
                                Left err -> print err
                                Right d -> do
                                    putStrLn "\n\nTyped:"
                                    prettyPrint d (0::Int)
                                    let e = cpsify ds c
                                    putStrLn "\n\nCPS Converted:"
                                    prettyPrint e (0::Int)
                                    let f = closureConvert e
                                    putStrLn "\n\nClosure Converted:"
                                    prettyPrint f (0::Int)
                                    let g = spill (regs config) f
                                    putStrLn "\n\nSpilled:"
                                    prettyPrint g (0::Int)