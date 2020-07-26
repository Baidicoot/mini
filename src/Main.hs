module Main where

import Parser.Parser
import Text.Parsec (parse, many)
import Checker.Kinds

import qualified Data.Map.Strict as Map

main =
    let (Right t) = parse typ "" "{m a, {s m}} -> a"
        (Right u) = desugarseq Map.empty t
        inf = infer u
        (Right k, map) = runKindInf inf empty in do
            putStr ("\"" ++ show t ++ "\" is of kind: ")
            print k
            print map