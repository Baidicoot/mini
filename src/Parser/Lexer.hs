module Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import Types.Ident
import Types.SExpr

name :: Parser Name
name = many1 (noneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ().")

whiteSpace :: Parser String
whiteSpace = many (oneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ")

whiteSep :: Parser a -> Parser a
whiteSep p = do
  a <- p
  whiteSpace
  pure a

ident :: Parser Identifier
ident = do
  is <- sepBy1 name (char '.')
  case is of
    [i] -> pure (LocalIdentifier i)
    _ -> pure (ExternalIdentifier (init is) (last is))

parens :: Parser a -> Parser a
parens p = do
  char '('
  a <- p
  char ')'
  pure a