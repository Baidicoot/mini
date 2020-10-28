module Types.Syntax where

import Types.Ident
import Types.Type
import Types.SExpr
import Types.Pattern
import Types.Graph
import Types.Prim

-- need to add source positions to syntax

data SyntaxNode
    = Ident Identifier
    | Lit UnboxedLit
    | LitTy LitType
    | Prim Primop
    | Arr
    | Ann
    | Star
    | Hole
    | Keyword String
    deriving(Eq)

instance Show SyntaxNode where
    show (Ident id) = show id
    show (Lit l) = show l
    show (Prim o) = show o
    show Arr = "->"
    show Ann = "::"
    show Star = "Ty"
    show Hole = "_"
    show (Keyword s) = s

type ExprS = SExpr SyntaxNode

data Match
    = Match Expr [(SourcePattern, Expr)]
    deriving(Eq, Show)

data Let
    = Let [ValDef] Expr
    deriving(Eq, Show)

data Fix
    = Fix [FunDef] Expr
    deriving(Eq, Show)

data Lam
    = Lam [Name] Expr
    deriving(Eq, Show)

data AnnotationPoly t a
    = Expl a t
    deriving(Eq, Show)

type Annotation = AnnotationPoly SourceType

data Data
    = Ind Name (Maybe SourceType) [Annotation Name]
    deriving(Eq, Show)

data ExprNode
    = Var Identifier
    | Annot (Annotation Expr)
    | LetIn Let
    | FixIn Fix
    | Lambda Lam
    | Literal UnboxedLit
    | Switch Match
    deriving(Eq, Show)

type Expr = SourceGraph ExprNode

data FunDef
    = FunDef (Maybe SourceType) Name [Name] Expr
    deriving(Eq, Show)

data ValDef
    = ValDef (Maybe SourceType) Name Expr
    deriving(Eq, Show)

data TopLevel
    = Func FunDef
    | Data Data
    deriving(Eq, Show)