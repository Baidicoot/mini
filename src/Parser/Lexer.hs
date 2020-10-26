module Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import Types.Ident
import Types.SExpr
import Types.Prim
import Types.Syntax

import Data.Functor

name :: Parser Name
name = many1 (noneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ().")

whiteSpace :: Parser String
whiteSpace = many (oneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b ")

whiteSep :: Parser a -> Parser a
whiteSep p = do
  a <- p
  whiteSpace
  pure a

int :: Parser Int
int = do
  s <- option "" (string "-")
  d <- many1 digit
  pure $ read (s ++ d)

charLit :: Parser Char
charLit = do
  char '\''
  c <- anyChar
  char '\''
  pure c

unit :: Parser UnboxedLit
unit = string "unit" $> Unit

literal :: Parser UnboxedLit
literal
  =   Int   <$> int
  <|> unit   $> Unit
  <|> Char  <$> charLit

literalTy :: Parser LitType
literalTy
  =   string "Unit" $>  UnitTy
  <|> string "Int"  $>  IntTy
  <|> string "Char" $>  CharTy

keyword :: Parser String
keyword
  =   string "let"
  <|> string "ind"
  <|> string "lam"
  <|> string "match"

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

rpnccNode :: Parser SyntaxNode
rpnccNode
  =   Lit         <$> try literal
  <|> Keyword     <$> keyword
  <|> string "->"  $> Arr
  <|> string "::"  $> Ann
  <|> string "Ty"  $> Star
  <|> string "_"   $> Hole
  <|> Ident       <$> ident