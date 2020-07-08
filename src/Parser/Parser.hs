module Parser.Parser where

import Parser.Syntax
import Parser.Lexer

import Control.Monad
import Data.Functor

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)

ttnamed :: Parser TypeToken
ttnamed = do
    i@(c:_) <- ident
    return (if isUpper c then TTNam i else TTVar i)

ttfun :: Parser TypeToken
ttfun = reserved "->" $> TTFun

ttseq :: Parser TypeToken
ttseq = do
    char '['
    whiteSpace
    tts <- (many typetoken) `sepBy` (char ',' >> whiteSpace)
    whiteSpace
    char ']'
    return (TTSeq tts)

typetoken :: Parser TypeToken
typetoken
    =   ttseq
    <|> fmap TTPar (parens (many typetoken))
    <|> ttfun
    <|> ttnamed
{-
enam :: Parser Expr
enam = fmap ENam ident

lam :: Parser Lambda
lam = do
    args <- many ident
    reserved "->"
    stmts <- many stmt
    return (Lambda args stmts)

pat :: Parser Pattern
pat = fmap PNam ident

pcase :: Parser ([Pattern], [Stmt])
pcase = do
    p <- many1 pat
    reserved "->"
    stmts <- many stmt
    return (p, stmts)

lcase :: Parser LCase
lcase = do
    reserved "case"
    fmap Case (many1 (parens pcase))

expr :: Parser Expr
expr
    =   enam
    <|> try (fmap ELam (parens lam))
    <|> fmap ECse (parens lcase)

spush :: Parser Stmt
spush = do
    char '\''
    fmap SPsh expr

sapp :: Parser Stmt
sapp = do
    char '*'
    fmap SApp expr

sexp :: Parser Stmt
sexp = fmap SExp expr

uletexpr :: Parser LetExpr
uletexpr = do
    n <- ident
    reserved ";"
    fmap (LetExpr n Nothing) stmts

tletexpr :: Parser LetExpr
tletexpr = do
    n <- ident
    reserved "::"
    t <- typ
    reserved ";"
    fmap (LetExpr n (Just t)) stmts

letexpr :: Parser LetExpr
letexpr
    =   try tletexpr
    <|> uletexpr

stmt :: Parser Stmt
stmt
    =   try (fmap SLet (parens letexpr))
    <|> spush
    <|> sapp
    <|> sexp

stmts :: Parser [Stmt]
stmts
    =   try (fmap ((: []) . SExp . ELam) lam)
    <|> many stmt

union :: Parser (String, [Type])
union = do
    n <- ident
    reserved "::"
    t <- typ
    return (n, t)

dat :: Parser Data
dat = do
    n <- ident
    reserved ";"
    cs <- many (parens union)
    return (Data n cs)

fun :: Parser Function
fun = do
    n <- ident
    reserved "::"
    t <- typ
    reserved ";"
    s <- stmts
    return (Func n t s)

export :: Parser a -> Parser a
export p = do
    reserved "export"
    p

toplevel :: Parser TopLevel
toplevel
    =   try (parens ef)
    <|> try (parens f)
    <|> try (parens ed)
    <|> parens d
    where
        f = fmap LocalF fun
        ef = fmap ExportF (export fun)
        d = fmap LocalD dat
        ed = fmap ExportD (export dat)
    
imprt :: Parser Declaration
imprt = do
    reserved "import"
    fmap Import ident

include :: Parser Declaration
include = do
    reserved "include"
    fmap Include ident

includeWith :: Parser Declaration
includeWith = do
    reserved "include"
    n <- ident
    ms <- parens (sepBy1 ident (reserved ","))
    return (IncludeWith n ms)

decl :: Parser Declaration
decl
    =   try (parens include)
    <|> try (parens includeWith)
    <|> try (parens imprt)
    <?> "import or include"

lib :: Parser Library
lib = do
    n <- parens (reserved "library" >> ident)
    ds <- many decl
    ts <- many toplevel
    return (Lib n ds ts)
-}