module Types.Syntax where

import Types.Ident
import Types.Type
import Types.SExpr
import Types.Pattern
import Types.Graph
import Types.Prim

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
    deriving(Eq, Show)

type ExprS = SExpr SyntaxNode

data Match
    = Match Expr [(Pattern, Expr)]
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

type Annotation = AnnotationPoly Type

data Data
    = Ind Name (Maybe Kind) [Annotation Name]
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

type Expr = AppGraph ExprNode

data FunDef
    = FunDef (Maybe Type) Name [Name] Expr
    deriving(Eq, Show)

data ValDef
    = ValDef (Maybe Type) Name Expr
    deriving(Eq, Show)

data TopLevel
    = Func FunDef
    | Data Data
    deriving(Eq, Show)