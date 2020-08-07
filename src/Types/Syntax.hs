module Types.Syntax where

import Types.Ident
import Types.Type
import Types.SExpr
import Types.Pattern
import Types.Graph

data Match
    = Match Expr [(Pattern, Expr)]
    deriving(Eq, Show)

data Let
    = Let [Definition] Expr
    deriving(Eq, Show)

data Lam
    = Lam [Name] Expr
    deriving(Eq, Show)

data AnnotationPoly t a
    = Expl a t
    deriving(Eq, Show)

type Annotation = AnnotationPoly Type

data Data
    = Ind Name (Maybe Kind) [Annotation Name]
    deriving(Eq, Show)

data Lit
    = Int Int
    deriving(Eq, Show)

data ExprNode
    = Var Identifier
    | Annot (Annotation Expr)
    | LetIn Let
    | Lambda Lam
    | Literal Lit
    | Switch Match
    deriving(Eq, Show)

type Expr = AppGraph ExprNode

data Definition
    = Defn (Maybe Type) Name [Name] Expr
    deriving(Eq, Show)

data TopLevel
    = Func Definition
    | Data Data
    deriving(Eq, Show)