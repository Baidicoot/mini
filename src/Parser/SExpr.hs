module Parser.SExpr where

import Parser.Lexer

import Control.Monad
import Data.Functor
import Control.Applicative (liftA2)

import Types.Ident
import Types.SExpr
import Types.Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Prim

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

sexpr :: Parser a -> Parser (SExpr a)
sexpr n
    =   sexp
    <|> rcrd
    <|> node
    where
        node = liftA2 SNode sourcePos (whiteSep n)
        sexp = liftA2 SExpr sourcePos (whiteSep . parens . many . sexpr $ n)
        rcrd = liftA2 SRcrd sourcePos (whiteSep . record . many . sexpr $ n)

rpncc :: Parser [ExprS]
rpncc = many (sexpr rpnccTok)