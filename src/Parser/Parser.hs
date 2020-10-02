module Parser.Parser where

import Data.Functor
import Control.Monad
import Control.Monad.Except
import Control.Applicative

import Types.SExpr
import Types.Ident
import Types.Syntax
import Types.Type
import Types.Pattern
import Types.Graph

import Data.Char (isLower)

type ExprS = SExpr Identifier

data SyntaxError
    = SyntaxError ExprS String
    | TopLevelVal Definition
    deriving(Eq, Show)

type Parser = Except [SyntaxError]

syntaxError :: ExprS -> String -> Parser a
syntaxError e s = throwError [SyntaxError e s]

parseapps :: (ExprS -> Parser a) -> ExprS -> Parser (AppGraph a)
parseapps p (SExpr (x:xs)) = do
    xexpr <- parseapp p x
    foldM (\acc x -> do
        exp <- parseapp p x
        pure (App NoTag acc exp)) xexpr xs
parseapps _ x = syntaxError x "app"

parseapp :: (ExprS -> Parser a) -> ExprS -> Parser (AppGraph a)
parseapp p x
    =   Node NoTag <$> p x
    <|> parseapps p x

class InfixArrow a where
    arrow :: a

parsearr :: (InfixArrow a) => (ExprS -> Parser (AppGraph a)) -> ExprS -> Parser (AppGraph a)
parsearr p (SExpr (x:xs)) = do
    xtyp <- p x
    parseinfixarr p xtyp xs
parsearr _ x = syntaxError x "arrow"

parseinfixarr :: (InfixArrow a) => (ExprS -> Parser (AppGraph a)) -> (AppGraph a) -> [ExprS] -> Parser (AppGraph a)
parseinfixarr p typ (SNode (LocalIdentifier "->"):x:xs) = do
    xtyp <- p x
    xstyp <- parseinfixarr p xtyp xs
    pure (App NoTag (App NoTag (Node NoTag arrow) typ) xstyp)
parseinfixarr _ typ [] = pure typ
parseinfixarr _ _ x = syntaxError (SExpr x) "infixArrow"

instance InfixArrow TypeNode where
    arrow = FunctionType

parseident :: ExprS -> Parser Identifier
parseident (SNode x) = pure x
parseident x = syntaxError x "identifier"

parselet :: ExprS -> Parser Let
parselet (SExpr (SNode (LocalIdentifier "let"):xs@(_:_))) = do
    defs <- mapM parsedefn (init xs)
    expr <- parseexpr (last xs)
    pure (Let defs expr)
parselet x = syntaxError x "let"

parsedefn :: ExprS -> Parser Definition
parsedefn (SExpr [n, args, def]) = do
    (name, typ) <- (
        (flip (,) Nothing) <$> parsename n
        <|> (\(Expl n t) -> (n, Just t)) <$> parseannot parsename n)
    argns <- parselist parsename args
    defexp <- parseexpr def
    pure (Defn typ name argns defexp)
parsedefn x = syntaxError x "definition"

parsedata :: ExprS -> Parser Data
parsedata (SExpr (SNode (LocalIdentifier "ind"):n:xs)) = do
    (name, kind) <- (
        (flip (,) Nothing) <$> parsename n
        <|> (\(Expl n t) -> (n, Just t)) <$> parseannotpoly parsekind parsename n)
    cases <- mapM (parseannot parsename) xs
    pure (Ind name kind cases)
parsedata x = syntaxError x "inductive"

parsename :: ExprS -> Parser Name
parsename (SNode (LocalIdentifier n)) = pure n
parsename x = syntaxError x "name"

parsevar :: ExprS -> Parser Name
parsevar x = do
    n <- parsename x
    case n of
        (c:_) | isLower c -> pure n
        _ -> syntaxError x "var"

parsereserved :: String -> ExprS -> Parser NoTag
parsereserved s (SNode (LocalIdentifier n))
    | s == n = pure NoTag
parsereserved s x = syntaxError x s

parselist :: (ExprS -> Parser a) -> ExprS -> Parser [a]
parselist p (SExpr xs) = mapM p xs
parselist p (SNode x) = mapM p [SNode x]

parselam :: ExprS -> Parser Lam
parselam (SExpr [SNode (LocalIdentifier "lam"), ns, expr]) = do
    argns <- parselist parsename ns
    defexp <- parseexpr expr
    pure (Lam argns defexp)
parselam x = syntaxError x "lambda"

parseannotpoly :: (ExprS -> Parser t) -> (ExprS -> Parser a) -> ExprS -> Parser (AnnotationPoly t a)
parseannotpoly t p (SExpr [SNode (LocalIdentifier "::"), exp, typ]) = do
    e <- p exp
    a <- t typ
    pure (Expl e a)
parseannotpoly _ _ x = syntaxError x "annotation"

parseannot :: (ExprS -> Parser a) -> ExprS -> Parser (Annotation a)
parseannot = parseannotpoly parsetype

parsepatternnode :: ExprS -> Parser PatternNode
parsepatternnode x
    =   parsereserved "_" x  $> PatternWildcard
    <|> PatternVar          <$> parsevar x
    <|> PatternCons         <$> parseident x

parsepattern :: ExprS -> Parser Pattern
parsepattern = parseapp parsepatternnode

parsecase :: ExprS -> Parser (Pattern, Expr)
parsecase (SExpr [p, e]) = do
    pat <- parsepattern p
    exp <- parseexpr e
    pure (pat, exp)
parsecase x = syntaxError x "case"

parsematch :: ExprS -> Parser Match
parsematch (SExpr ((SNode (LocalIdentifier "match")):e:ms)) = do
    expr <- parseexpr e
    cases <- mapM parsecase ms
    pure (Match expr cases)
parsematch x = syntaxError x "match"

parseexprnode :: ExprS -> Parser ExprNode
parseexprnode exp
    =   Lambda  <$> parselam exp
    <|> LetIn   <$> parselet exp
    <|> Switch  <$> parsematch exp
    <|> Annot   <$> parseannot parseexpr exp
    <|> Var     <$> parseident exp

parseexpr :: ExprS -> Parser Expr
parseexpr = parseapp parseexprnode

parseparensarr :: ExprS -> Parser TypeNode
parseparensarr (SExpr [SNode (LocalIdentifier "->")]) = pure FunctionType
parseparensarr x = syntaxError x "parensArrow"

-- when not busy, refactor to use `parsevar`
parsenamedtype :: ExprS -> Parser TypeNode
parsenamedtype x@(SNode (LocalIdentifier "->")) = syntaxError x "namedType"
parsenamedtype (SNode id@(LocalIdentifier s@(c:_)))
    | isLower c = pure (TypeVar s)
    | otherwise = pure (NamedType id)
parsenamedtype (SNode id) = pure (NamedType id)
parsenamedtype x = syntaxError x "namedType"

parsetypenode :: ExprS -> Parser TypeNode
parsetypenode x
    =   parsenamedtype x
    <|> parseparensarr x

parsetype :: ExprS -> Parser Type
parsetype x
    =   parsearr parsetype x
    <|> parseapp parsetypenode x

parsekindnode :: ExprS -> Parser TypeNode
parsekindnode x
    =   parsereserved "*" x $> KindStar

parsekind :: ExprS -> Parser Kind
parsekind x
    =   parsearr parsekind x
    <|> parseapp parsekindnode x

func :: Definition -> Parser TopLevel
func d@(Defn _ _ [] _) = throwError [TopLevelVal d]
func d = pure $ Func d

parsetoplevel :: [ExprS] -> Parser [TopLevel]
parsetoplevel = mapM (\x
    ->  (parsedefn x >>= func)
    <|> Data <$> parsedata x)

runParse :: Parser x -> Either [SyntaxError] x
runParse = runExcept