{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Parser.Syntax
import Parser.Lexer

import Control.Monad
import Data.Functor
import Data.List (foldl')
import Data.Char

import Types.Type
import Types.Kind
import Types.Ident

import Text.Parsec
import Text.Parsec.String (Parser)

import Prelude hiding(mod)

kother :: Parser Kind
kother
    =   parens kind
    <|> reserved "Seq" $> KindSeq
    <|> reserved "*" $> KindStar
    <|> reserved "@" $> KindAny

kfunc :: Parser Kind
kfunc = do
    ks <- kother
    reserved "->"
    (KindFunc ks) <$> kind

kind :: Parser Kind
kind
    =   try kfunc
    <|> kother

ttnamed :: Parser TypeToken
ttnamed = do
    i <- ident
    case i of
        ExternalIdentifier _ _ -> pure (TTNam i)
        LocalIdentifier (c:_) | isUpper c -> pure (TTNam i)
        LocalIdentifier s -> pure (TTVar s)

ttfun :: Parser TypeToken
ttfun = reserved "->" $> TTFun

ttseq :: Parser TypeToken
ttseq = do
    whiteSpace
    char '{'
    whiteSpace
    tts <- (many typetoken) `sepBy` (char ',' >> whiteSpace)
    whiteSpace
    char '}'
    whiteSpace
    return (TTSeq tts)

typetoken :: Parser TypeToken
typetoken
    =   ttseq
    <|> fmap TTPar (parens (many typetoken))
    <|> ttfun
    <|> ttnamed

enam :: Parser Expr
enam = fmap ENam ident

lam :: Parser Lambda
lam = do
    args <- many name
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

sexp :: Parser Stmt
sexp = fmap SExp expr

uletexpr :: Parser LetExpr
uletexpr = do
    n <- name
    reserved ";"
    fmap (LetExpr n Nothing) stmts

tletexpr :: Parser LetExpr
tletexpr = do
    n <- name
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
    <|> sexp

stmts :: Parser [Stmt]
stmts
    =   try (fmap ((: []) . SExp . ELam) lam)
    <|> many stmt

union :: Parser (String, Type)
union = do
    n <- name
    reserved "::"
    t <- typ
    return (n, t)

dat :: Parser Data
dat = do
    n <- name
    reserved "::"
    k <- kind
    reserved ";"
    cs <- many (parens union)
    return (Data n k cs)

fun :: Parser Function
fun = do
    n <- name
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

mod :: Parser Module
mod = many name

imprt :: Parser Declaration
imprt = do
    reserved "import"
    fmap Import mod

include :: Parser Declaration
include = do
    reserved "include"
    fmap Include mod

includeWith :: Parser Declaration
includeWith = do
    reserved "include"
    n <- mod
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

getLocalKinds :: Library -> [(String, Kind)]
getLocalKinds (Lib _ _ ts) = map (\case
    LocalD (Data n k _) -> (n, k)
    ExportD (Data n k _) -> (n, k)) datas
    where
        datas = filter (\case
            (LocalD _) -> True
            (ExportD _) -> True
            _ -> False) ts

getExportKinds :: Library -> [(String, Kind)]
getExportKinds (Lib _ _ ts) = map (\case
    ExportD (Data n k _) -> (n, k)) datas
    where
        datas = filter (\case
            (ExportD _) -> True
            _ -> False) ts

convtype :: TypeToken -> Either String Type
convtype TTFun = Right FunctionType
convtype (TTNam s) = Right (NamedType s)
convtype (TTVar s) = Right (TypeVar s)
convtype (TTPar xs) = parsetype xs
convtype (TTSeq [[]]) = Right (SeqType [])
convtype (TTSeq xss) = fmap SeqType (mapM parsetype xss)

apps :: [Type] -> Type -> Type
apps (x:xs) = TypeApp (foldl' TypeApp x xs)
apps _ = id

parsetype :: [TypeToken] -> Either String Type
parsetype [] = Left "empty type not allowed"
parsetype (x:xs) = convtype x >>= (\x -> internal [x] xs)
    where
        internal (s:ss) (TTFun:t:ts) = convtype t >>= (\t -> internal (t:TypeApp FunctionType s:ss) ts)
        internal (s:ss) (t:ts) = convtype t >>= (\t -> internal (TypeApp s t:ss) ts)
        internal ss [] = Right (foldl1 (flip TypeApp) ss)

typ :: Parser Type
typ = do
    ts <- many typetoken
    case parsetype ts of
        Right x -> pure x
        Left err -> parserFail (err ++ " while parsing type " ++ show ts)