module Types.CPS where

import Types.Ident
import Types.Type
import Types.Literal

data Value
    = Var Identifier
    | Label Identifier
    | Unboxed UnboxedLit
    deriving(Eq, Show)

data CFun
    = Fun Name [Name] CExp
    deriving(Eq, Show)

data AccessPath
    = OffPath Int
    | SelPath Int AccessPath
    deriving(Eq, Show)

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Identifier CExp
    | Select Int Value Identifier CExp
    | Offset Int Value Identifier CExp
    | Switch Value [CExp]
    | Primop Primop [Value] [Identifier] [CExp]
    deriving(Eq, Show)