module Error.Error where

import Text.Parsec.Pos

getLine :: Int -> String -> String
getLine 0 s = takeWhile (/= '\n') s
getLine x s = internal (x-1) . drop 1 . dropWhile (/= '\n')

class RenderableError e where
    errPos :: e -> SourcePos
    errType :: e -> String
    errCont :: e -> [String]

render :: (RenderableError e) => String -> e -> String