module Types.Syntax where

import Types.Ident
import Types.Type
import Types.SExpr
import Types.Pattern
import Types.Graph
import Types.Prim

import Text.Parsec.Pos

data SyntaxNode
    = Ident Identifier
    | SynLit UnboxedLit
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
    show (SynLit l) = show l
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
    | Tuple [Expr]
    | Select Int Expr
    | Annot (Annotation Expr)
    | LetIn Let
    | FixIn Fix
    | Lambda Lam
    | Literal UnboxedLit
    | Switch Match
    | Primop Primop
    deriving(Eq, Show)

type Expr = SourceGraph ExprNode

data FunDef
    = FunDef SourcePos (Maybe SourceType) Name [Name] Expr
    deriving(Eq, Show)

data ValDef
    = ValDef SourcePos (Maybe SourceType) Name Expr
    deriving(Eq, Show)

data TopLevel
    = Group SourcePos [FunDef]
    | Vals SourcePos [ValDef]
    | Data SourcePos Data
    deriving(Eq, Show)