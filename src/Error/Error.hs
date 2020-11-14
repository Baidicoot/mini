module Error.Error where

import Data.List (intercalate)
import Text.Parsec.Pos

getSrcLine :: Int -> String -> String
getSrcLine 0 s = takeWhile (/= '\n') s
getSrcLine x s = getSrcLine (x-1) . drop 1 . dropWhile (/= '\n') $ s

class RenderableError e where
    errPos :: e -> SourcePos
    errType :: e -> String
    errCont :: e -> [String]

render :: (RenderableError e) => String -> e -> String
render t e =
    let pos = errPos e
        row = sourceLine pos - 1
        col = sourceColumn pos - 1
        file = sourceName pos
        typ = errType e
        line = getSrcLine row t
        cont = errCont e
    in
        typ
        ++ " in module '" ++ file ++ "', row " ++ show row ++ " column " ++ show col ++ ":\n"
        ++ line ++ "\n"
        ++ replicate col ' '
        ++ "^-- " ++ intercalate ("\n" ++ replicate col ' ') cont