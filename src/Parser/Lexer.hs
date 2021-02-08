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
name = many1 (noneOf "\n\r\t\v\f\x00a0\x1680\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x200b (){}#.")

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

sel :: Parser Int
sel = whiteSep $ char '#' >> read <$> many1 digit

charLit :: Parser Char
charLit = do
  char '\''
  c <- anyChar
  char '\''
  pure c

comment :: Parser ()
comment = whiteSep $ do
  string "(."
  manyTill anyChar (try (string ".)"))
  pure ()

literal :: Parser UnboxedLit
literal
  =   Int   <$> int
  <|> Char  <$> charLit

literalTy :: Parser LitType
literalTy
  =   reserved "Int"  $>  IntTy
  <|> reserved "Char" $>  CharTy
  <|> reserved "Ref"  $>  RefTy

keyword :: Parser String
keyword
  =   try (reserved "let")
  <|> try (reserved "ind")
  <|> try (reserved "lam")
  <|> try (reserved "match")
  <|> try (reserved "fix")
  <|> try (reserved "import-as")
  <|> try (reserved "include")

primop :: Parser Primop
primop
  =   try (reserved "+")      $> AAdd
  <|> try (reserved "-")      $> ASub
  <|> try (reserved "*")      $> AMul
  <|> try (reserved "/")      $> ADiv
  <|> try (reserved "putint") $> PutInt
  <|> try (reserved "putchr") $> PutChr
  <|> try (reserved "cmpint") $> CmpInt
  <|> try (reserved "cmpchr") $> CmpChar
  <|> try (reserved "eqint")  $> EqInt
  <|> try (reserved "eqchr")  $> EqChar
  <|> try (reserved "ord")    $> CharToInt
  <|> try (reserved "chr")    $> IntToChar
  <|> try (reserved "setref") $> SetRef
  <|> try (reserved "newref") $> NewRef
  <|> try (reserved "getref") $> GetRef

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

record :: Parser a -> Parser a
record p = do
  char '{'
  as <- p
  char '}'
  pure as

rpnccTok :: Parser SyntaxNode
rpnccTok
  = SynLit                <$> try literal
  <|> LitTy               <$> try literalTy
  <|> Keyword             <$> try keyword
  <|> try (reserved "->")  $> Arr
  <|> try (reserved "::")  $> Ann
  <|> try (reserved "Ty")  $> Star
  <|> try (reserved "_")   $> Hole
  <|> Sel                 <$> sel
  <|> Prim                <$> try primop
  <|> Ident               <$> ident