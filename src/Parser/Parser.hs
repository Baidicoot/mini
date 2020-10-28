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
import Text.Parsec.Pos

data SyntaxError
    = Expecting String String SourcePos

instance Show SyntaxError where
    show (Expecting a b p) = "'" ++ sourceName p ++ "': expecting " ++ a ++ ", got " ++ b ++ ", at line " ++ show (sourceLine p) ++ ", column " ++ show (sourceColumn p)
    showList xs = ((concatMap ((++"\n") . show) xs)++)

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

many1 :: String -> SourcePos -> Parser s a -> Parser [s] [a]
many1 _ _ p xs@(_:_) = many p xs
many1 s p _ [] = Left [Expecting ("at least one " ++ s) "nothing" p]

manyEnd :: String -> SourcePos -> Parser s a -> Parser s b -> Parser [s] ([a], b)
manyEnd _ _ a b xs@(_:_) = both (many a) b (init xs, last xs)
manyEnd s p _ _ [] = Left [Expecting s "nothing" p]

expr :: ExprParser Expr
expr (SExpr _ (x:SNode p Ann:xs)) = Node p . Annot <$> annot expr (x, SExpr p xs)
expr (SExpr _ (SNode p (Keyword "let"):xs)) = Node p . LetIn <$> letexp p xs
expr (SExpr _ (SNode p (Keyword "fix"):xs)) = Node p . FixIn <$> fixexp p xs
expr (SExpr _ (SNode p (Keyword "lam"):xs)) = Node p . Lambda <$> lamexp p xs
expr (SExpr _ (SNode p (Keyword "match"):xs)) = Node p . Switch <$> matchexp p xs
expr (SNode p (Lit l)) = Right (Node p $ Literal l)
expr (SNode p (Ident i)) = Right (Node p $ Var i)
expr (SExpr p xs) = apps "expression" p expr xs
expr s = Left [Expecting "expression" (display s) (getPos s)]

annot :: ExprParser a -> Parser (ExprS, ExprS) (Annotation a)
annot a (x, xs) = uncurry Expl <$> both a typeexp (x, xs)

letexp :: SourcePos -> Parser [ExprS] Let
letexp p = fmap (uncurry Let) . manyEnd "expression" p valdef expr

fixexp :: SourcePos -> Parser [ExprS] Fix
fixexp p = fmap (uncurry Fix) . manyEnd "expression" p fundef expr

lamexp :: SourcePos -> Parser [ExprS] Lam
lamexp _ (x:xs) = uncurry Lam <$> both args expr (x, SExpr (getPos x) xs)
lamexp p x = Left [Expecting "lambda" "nothing" p]

matchexp :: SourcePos -> Parser [ExprS] Match
matchexp _ (x:xs) = uncurry Match <$> both expr (many caseexp) (x, xs)
matchexp p x = Left [Expecting "expression" "nothing" p]

apps :: String -> SourcePos -> Parser ExprS (SourceGraph a) -> Parser [ExprS] (SourceGraph a)
apps _ _ p [s] = p s
apps s o p (x:xs) = uncurry (App o) <$> both p (apps s (getPos x) p) (x, xs)
apps s p _ x = Left [Expecting (s ++ " application") "nothing" p]

typeexp :: ExprParser SourceType
typeexp (SExpr _ (x:SNode p Arr:xs)) = uncurry (fnTag p) <$> both typeexp typeexp (x, SExpr p xs)
typeexp (SNode p (Ident (LocalIdentifier i@(c:_))))
    | isLower c = Right (Node p (TypeVar i))
typeexp (SNode p (Ident i)) = Right (Node p (NamedType i))
typeexp (SNode p (LitTy t)) = Right (Node p (Builtin t))
typeexp (SNode p Star) = Right (Node p KindStar)
typeexp (SExpr p xs) = apps "type" p typeexp xs
typeexp x = Left [Expecting "type" (display x) (getPos x)]

valdef :: ExprParser ValDef
valdef (SExpr _ (d:xs)) = (\((mt,n),e) -> ValDef mt n e) <$> both decl expr (d, SExpr (getPos d) xs)
valdef x = Left [Expecting "value definition" (display x) (getPos x)]

fundef :: ExprParser FunDef
fundef (SExpr _ (f:a:xs))
    =   (\(((mt,n),a),e) -> FunDef mt n a e)
    <$> both (both decl args) expr ((f, a), SExpr (getPos a) xs)
fundef x = Left [Expecting "function definition" (display x) (getPos x)]

args :: ExprParser [Name]
args (SNode _ (Ident (LocalIdentifier i))) = Right [i]
args (SExpr p xs) = many1 "name" p name xs
args x = Left [Expecting "arguments" "nothing" (getPos x)]

caseexp :: ExprParser (SourcePattern, Expr)
caseexp (SExpr pp xs) = (\(p,e,ep) -> both patexp expr (SExpr pp p, SExpr ep e)) =<< splitArr pp xs
    where
        splitArr _ ((SNode ep Arr):xs) = Right ([], xs, ep)
        splitArr _ (x:xs) = (\(p, e, ep) -> (x:p, e, ep)) <$> splitArr (getPos x) xs
        splitArr p _ = Left [Expecting "::" "nothing" p]
caseexp x = Left [Expecting "case" (display x) (getPos x)]

name :: ExprParser Name
name (SNode _ (Ident (LocalIdentifier l))) = Right l
name x = Left [Expecting "name" (display x) (getPos x)]

decl :: ExprParser (Maybe SourceType, Name)
decl (SNode _ (Ident (LocalIdentifier l))) = Right (Nothing, l)
decl (SExpr _ (SNode _ (Ident (LocalIdentifier l)):SNode p Ann:xs)) = (\t -> (Just t, l)) <$> typeexp (SExpr p xs)
decl x = Left [Expecting "declaration" (display x) (getPos x)]

patexp :: ExprParser SourcePattern
patexp (SNode p (Ident (LocalIdentifier l@(c:_))))
    | isLower c = Right (Node p $ PatternVar l)
patexp (SNode p (Ident i)) = Right (Node p $ PatternCons i)
patexp (SNode p Hole) = Right (Node p PatternWildcard)
patexp (SExpr p xs) = apps "pattern" p patexp xs
patexp x = Left [Expecting "pattern" (display x) (getPos x)]

indexp :: SourcePos -> Parser [ExprS] Data
indexp _ (n:xs) = (\((mt, n), ds) -> Ind n mt ds) <$> both decl (many (annotation "name" name)) (n, xs)
indexp p x = Left [Expecting "inductive" "nothing" p]

annotation :: String -> ExprParser a -> ExprParser (Annotation a)
annotation _ p (SExpr _ (x:SNode pos Ann:xs)) = annot p (x, SExpr pos xs)
annotation s _ x = Left [Expecting ("annotated " ++ s) (display x) (getPos x)]

toplevel :: Parser ExprS TopLevel
toplevel (SExpr _ (SNode p (Keyword "ind"):xs)) = Data <$> indexp p xs
toplevel x = Func <$> fundef x

toplevelexpr :: Parser [ExprS] [TopLevel]
toplevelexpr = many toplevel