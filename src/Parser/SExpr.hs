module Parser.SExpr where

import Parser.Lexer

import Control.Monad
import Data.Functor

import Types.Ident
import Types.SExpr

import Text.Parsec
import Text.Parsec.String (Parser)

sexpr :: Parser a -> Parser (SExpr a)
sexpr n
    =   sexp
    <|> node
    where
        node = fmap SNode (whiteSep n)
        sexp = fmap SExpr (whiteSep . parens . many . sexpr $ n)

rpnccNode :: Parser Identifier
rpnccNode = ident

rpncc :: Parser (SExpr Identifier)
rpncc = sexpr rpnccNode