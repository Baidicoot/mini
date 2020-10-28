{-# LANGUAGE LambdaCase #-}

module Parser.Parser (toplevelexpr) where

import Prelude hiding(or)

import Data.Monoid
import Data.Functor
import Control.Monad
import Data.Either

import Types.SExpr
import Types.Ident
import Types.Syntax
import Types.Type
import Types.Prim
import Types.Pattern
import Types.Graph

import Data.Char (isLower)

data SyntaxError
    = Expecting String String
    deriving(Show, Eq)

type Parser s a = s -> Either [SyntaxError] a
type ExprParser a = Parser ExprS a

bothE :: Monoid m => Either m a -> Either m b -> Either m (a,b)
bothE (Left m) (Left n) = Left (mappend m n)
bothE (Left m) _ = Left m
bothE _ (Left n) = Left n
bothE (Right a) (Right b) = Right (a, b)

both :: Parser s0 a -> Parser s1 b -> Parser (s0, s1) (a, b)
both a b (s0, s1) = bothE (a s0) (b s1)

many :: Parser s a -> Parser [s] [a]
many p xs = foldr (\a b -> fmap (uncurry (:)) $ bothE (p a) b) (Right []) xs

many1 :: String -> Parser s a -> Parser [s] [a]
many1 _ p xs@(_:_) = many p xs
many1 s _ [] = Left [Expecting ("at least one " ++ s) "none"]

manyEnd :: String -> Parser s a -> Parser s b -> Parser [s] ([a], b)
manyEnd _ a b xs@(_:_) = both (many a) b (init xs, last xs)
manyEnd s _ _ [] = Left [Expecting s "none"]

expr :: ExprParser Expr
expr (SExpr (x:SNode Ann:xs)) = Node NoTag . Annot <$> annot expr (x, xs)
expr (SExpr (SNode (Keyword "let"):xs)) = Node NoTag . LetIn <$> letexp xs
expr (SExpr (SNode (Keyword "fix"):xs)) = Node NoTag . FixIn <$> fixexp xs
expr (SExpr (SNode (Keyword "lam"):xs)) = Node NoTag . Lambda <$> lamexp xs
expr (SExpr (SNode (Keyword "match"):xs)) = Node NoTag . Switch <$> matchexp xs
expr (SNode (Lit l)) = Right (Node NoTag $ Literal l)
expr (SNode (Ident i)) = Right (Node NoTag $ Var i)
expr (SExpr xs) = apps "expression" expr xs
expr s = Left [Expecting "expression" (show s)]

annot :: ExprParser a -> Parser (ExprS, [ExprS]) (Annotation a)
annot a (x, xs) = uncurry Expl <$> both a typeexp (x, SExpr xs)

letexp :: Parser [ExprS] Let
letexp = fmap (uncurry Let) . manyEnd "expression" valdef expr

fixexp :: Parser [ExprS] Fix
fixexp = fmap (uncurry Fix) . manyEnd "expression" fundef expr

lamexp :: Parser [ExprS] Lam
lamexp (x:xs) = uncurry Lam <$> both args expr (x, SExpr xs)
lamexp x = Left [Expecting "lambda" (show x)]

matchexp :: Parser [ExprS] Match
matchexp (x:xs) = uncurry Match <$> both expr (many caseexp) (x, xs)
matchexp x = Left [Expecting "expression" (show x)]

apps :: String -> Parser s (AppGraph a) -> Parser [s] (AppGraph a)
apps _ p [s] = p s
apps s p (x:xs) = uncurry (App NoTag) <$> both p (apps s p) (x, xs)
apps s _ x = Left [Expecting (s ++ " application") "nothing"]

typeexp :: ExprParser Type
typeexp (SExpr (x:SNode Arr:xs)) = uncurry (-->) <$> both typeexp typeexp (x, SExpr xs)
typeexp (SNode (Ident (LocalIdentifier i@(c:_))))
    | isLower c = Right (Node NoTag (TypeVar i))
typeexp (SNode (Ident i)) = Right (Node NoTag (NamedType i))
typeexp (SNode (LitTy t)) = Right (Node NoTag (Builtin t))
typeexp (SNode Star) = Right (Node NoTag KindStar)
typeexp (SExpr xs) = apps "type" typeexp xs
typeexp x = Left [Expecting "type" (show x)]

valdef :: ExprParser ValDef
valdef (SExpr (d:xs)) = (\((mt,n),e) -> ValDef mt n e) <$> both decl expr (d, SExpr xs)
valdef x = Left [Expecting "value definition" (show x)]

fundef :: ExprParser FunDef
fundef (SExpr (f:a:xs))
    =   (\(((mt,n),a),e) -> FunDef mt n a e)
    <$> both (both decl args) expr ((f, a), SExpr xs)
fundef x = Left [Expecting "function definition" (show x)]

args :: ExprParser [Name]
args (SNode (Ident (LocalIdentifier i))) = Right [i]
args (SExpr xs) = many1 "name" name xs

caseexp :: ExprParser (Pattern, Expr)
caseexp (SExpr xs) = let (p, e) = splitArr xs in both patexp expr (SExpr p, SExpr e)
    where
        splitArr ((SNode Arr):xs) = ([], xs)
        splitArr (x:xs) = let (p, e) = splitArr xs in (x:p, e)
        splitArr _ = ([], [])
caseexp x = Left [Expecting "case" (show x)]

name :: ExprParser Name
name (SNode (Ident (LocalIdentifier l))) = Right l
name x = Left [Expecting "name" (show x)]

decl :: ExprParser (Maybe Type, Name)
decl (SNode (Ident (LocalIdentifier l))) = Right (Nothing, l)
decl (SExpr (SNode (Ident (LocalIdentifier l)):SNode Ann:xs)) = (\t -> (Just t, l)) <$> typeexp (SExpr xs)
decl x = Left [Expecting "declaration" (show x)]

patexp :: ExprParser Pattern
patexp (SNode (Ident (LocalIdentifier l@(c:_))))
    | isLower c = Right (Node NoTag $ PatternVar l)
patexp (SNode (Ident i)) = Right (Node NoTag $ PatternCons i)
patexp (SNode Hole) = Right (Node NoTag PatternWildcard)
patexp (SExpr xs) = apps "pattern" patexp xs
patexp x = Left [Expecting "pattern" (show x)]

indexp :: Parser [ExprS] Data
indexp (n:xs) = (\((mt, n), ds) -> Ind n mt ds) <$> both decl (many (annotation "name" name)) (n, xs)
indexp x = Left [Expecting "inductive" (show x)]

annotation :: String -> ExprParser a -> ExprParser (Annotation a)
annotation _ p (SExpr (x:SNode Ann:xs)) = annot p (x, xs)
annotation s _ x = Left [Expecting ("annotated " ++ s) (show x)]

toplevel :: Parser ExprS TopLevel
toplevel (SExpr (SNode (Keyword "ind"):xs)) = Data <$> indexp xs
toplevel x = Func <$> fundef x

toplevelexpr :: Parser [ExprS] [TopLevel]
toplevelexpr = many toplevel