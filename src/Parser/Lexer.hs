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

reserved :: String -> Parser String
reserved = whiteSep . string

int :: Parser Int
int = whiteSep $ do
  s <- option "" (string "-")
  d <- many1 digit
  pure $ read (s ++ d)

charLit :: Parser Char
charLit = do
  char '\''
  c <- anyChar
  char '\''
  pure c

literal :: Parser UnboxedLit
literal
  =   Int   <$> int
  <|> Char  <$> charLit

literalTy :: Parser LitType
literalTy
  =   reserved "Int"  $>  IntTy
  <|> reserved "Char" $>  CharTy

keyword :: Parser String
keyword
  =   try (reserved "let")
  <|> try (reserved "ind")
  <|> reserved "lam"
  <|> reserved "match"
  <|> reserved "fix"
  <|> reserved "import-as"

primop :: Parser Primop
primop
  =   try (reserved "+")      $> AAdd
  <|> try (reserved "-")      $> ASub
  <|> try (reserved "putint") $> PutInt
  <|> try (reserved "putchr") $> PutChr

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

rpnccTok :: Parser SyntaxNode
rpnccTok
  =   SynLit              <$> try literal
  <|> LitTy               <$> try literalTy
  <|> Keyword             <$> try keyword
  <|> try (reserved "->")  $> Arr
  <|> try (reserved "::")  $> Ann
  <|> try (reserved "Ty")  $> Star
  <|> try (reserved "_")   $> Hole
  <|> Prim                <$> try primop
  <|> Ident               <$> ident