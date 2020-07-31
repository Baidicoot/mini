module Parser.Syntax where
import Types.Type
import Types.Kind
import Types.Ident
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
    | TTNam Identifier
    | TTVar Name
    | TTPar [TypeToken]
    | TTSeq [[TypeToken]]
    deriving(Eq, Show)

data Expr
    = ENam Identifier
    | ELam Lambda
    | ECse LCase
    deriving(Eq, Show)

data Stmt
    = SExp Expr
    | SPsh Expr
    | SLet LetExpr
    deriving(Eq, Show)

data Lambda
    = Lambda [Name] [Stmt]
    deriving(Eq, Show)

data PatternTok
    = PNam Identifier
    | PVar Name
    | PAny
    deriving(Eq, Show)

data LCase
    = Case [([PatternTok], [Stmt])]
    deriving(Eq, Show)

data LetExpr
    = LetExpr Name (Maybe Type) [Stmt]
    deriving(Eq, Show)

data Data
    = Data Name Kind [(Name, Type)]
    deriving(Eq, Show)

data Function
    = Func Name Type [Stmt]
    deriving(Eq, Show)

data TopLevel
    = LocalF Function
    | ExportF Function
    | LocalD Data
    | ExportD Data
    deriving(Eq, Show)

data Declaration
    = Import Module
    | Include Module
    | IncludeWith Module [Identifier]
    deriving(Eq, Show)

data Library
    = Lib Identifier [Declaration] [TopLevel]
    deriving(Eq, Show)
