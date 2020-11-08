module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse, many)

import Types.Type
import Types.Ident
import Types.Graph
import Types.Pretty
--import Types.IR
import Types.Core
import Types.Env
import Types.Abstract

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

--import Frontend.IRify
--import Frontend.GenEnv
--import Frontend.Constraint
--import Frontend.Solve
import CPS.CPSify
import Elaborate.Elaborate
import TypeCheck.Check
import CPS.ClosureConv
import CPS.Spill
import CPS.Meta
import Backend.AbstGen

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
    case parse (many rpncc) "repl" line of
        Left err -> print err
        Right a -> case toplevelexpr a of
            Left err -> print err
            Right b -> case elaborate 0 ["Repl"] mempty b of
                        Left (e,w) -> do
                            putStrLn "elaboration failed with:"
                            print e
                            putStrLn "warnings:"
                            print w
                        Right (c, g, s0, w) -> do
                            putStrLn "elaboration succeded with warnings:"
                            print w
                            putStrLn "resulting in:"
                            print c
                            let consenv = importWithAction include (includeGADTs ["Repl"] g)
                            case typecheck s0 consenv c of
                                Left e -> do
                                    putStrLn "typecheck failed with:"
                                    print e
                                Right (d,s1) -> do
                                    putStrLn "typecheck resulted in:"
                                    prettyPrint d (0::Int)
                                    let (e, s2) = cpsify mempty (untagCore d) s1
                                    putStrLn "\n\nCPS Converted:"
                                    prettyPrint e (0::Int)
                                    let f = closureConvert e s2
                                    putStrLn "\n\nClosure Converted:"
                                    prettyPrint f (0::Int)
                                    let g = spill (regs config) f
                                    putStrLn "\n\nSpilled:"
                                    prettyPrint g (0::Int)
                                    let h = generateAbstract g (regs config)
                                    putStrLn "\n\nAbstract:"
                                    print h