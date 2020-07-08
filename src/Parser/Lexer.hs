module Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
    where
        names = ["export", "import", "include", "library", "case", ";", "->", "::"]
        style = emptyDef
            { T.commentStart      = "{-"
            , T.commentEnd        = "-}"
            , T.commentLine       = "--"
            , T.identLetter       = noneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ()[]^',;"
            , T.identStart        = noneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ()[]^',;"
            , T.opStart           = oneOf []
            , T.opLetter          = oneOf []
            , T.reservedNames     = names
            }

reserved :: String -> Parser ()
reserved = T.reserved lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

ident :: Parser String
ident = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r

integer :: Parser Integer
integer = T.integer lexer