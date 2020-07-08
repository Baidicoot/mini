module Parser.Syntax where

{-
rpnccVI Grammar Specification

== non-frozen, subject to change ==

type
    = type*             -- sequence type
    | "[" type* "]"     -- explicit seq. type
    | IDENT             -- single type
    | IDENT"["type+"]"  -- application type
    | type "->" type    -- function type
    | "(" type ")"      -- parens type

expr
    = IDENT
    | OP
    | plambda
    | plcase

stmt
    = expr
    | "'"expr
    | "^"expr
    | letexpr

plambda
    = "(" lambda ")"

lambda
    = IDENT* "->" stmt*

plcase
    = "(" lcase ")"

lcase
    = "case" ("(" pattern "->" stmt* ")")+

pattern
    = OP
    | IDENT
    | pattern+

letexpr
    = "(" IDENT "::" type+ ";" stmt* ")"
    | "(" IDENT ";" stmt* ")"
    | "(" IDENT ";" lambda ")"

data
    = "(" OP ";" ( "(" OP "::" type* ")" )* ")"
    | "(" EXPORT OP ";" ( "(" OP "::" type* ")" )* ")"

toplevel
    = "(" IDENT "::" type* ";" stmt* ")"
    | "(" EXPORT IDENT "::" type* ";" stmt* ")"
    | data

declaration
    = "(" IMPORT IDENT ")"
    | "(" INCLUDE IDENT ")"
    | "(" INCLUDE IDENT USING IDENT+ ")"
    | "(" LIBRARY IDENT ")"
-}

data TypeToken
    = TTFun
    | TTNam String
    | TTVar String
    | TTPar [TypeToken]
    | TTSeq [[TypeToken]]
    deriving(Eq, Show)

data Type
    = TypeApp Type [Type]
    | FunctionType
    | NamedType String
    | TypeVar String
    deriving(Eq, Show)

data Expr
    = ENam String
    | ELam Lambda
    | ECse LCase
    deriving(Eq, Show)

data Stmt
    = SExp Expr
    | SPsh Expr
    | SApp Expr
    | SLet LetExpr
    deriving(Eq, Show)

data Lambda
    = Lambda [String] [Stmt]
    deriving(Eq, Show)

data Pattern
    = PNam String
    deriving(Eq, Show)

data LCase
    = Case [([Pattern], [Stmt])]
    deriving(Eq, Show)

data LetExpr
    = LetExpr String (Maybe Type) [Stmt]
    deriving(Eq, Show)

data Data
    = Data String [(String, Type)]
    deriving(Eq, Show)

data Function
    = Func String Type [Stmt]
    deriving(Eq, Show)

data TopLevel
    = LocalF Function
    | ExportF Function
    | LocalD Data
    | ExportD Data
    deriving(Eq, Show)

data Declaration
    = Import String
    | Include String
    | IncludeWith String [String]
    deriving(Eq, Show)

data Library
    = Lib String [Declaration] [TopLevel]
    deriving(Eq, Show)