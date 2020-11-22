module Elaborate.Elaborator where

import Types.Pattern
import Types.Core
import Types.Ident
import Types.Type

import Control.Monad.Errors
import Control.Monad
import Control.Monad.RWS

import Error.Error

import Text.Parsec.Pos

import Data.List (intercalate)

import qualified Data.Map as Map

data ElabError
    = UnboundTerm SourcePos Identifier
    | UnboundType SourcePos Identifier
    | Conflicting SourcePos Name
    deriving(Show)

instance RenderableError ElabError where
    errPos (UnboundTerm p _) = p
    errPos (UnboundType p _) = p
    errPos (Conflicting p _) = p

    errType _ = "source"

    errCont (UnboundTerm _ i) = ["unbound identifier '" ++ show i ++ "'"]
    errCont (UnboundType _ i) = ["unbound named type '" ++ show i ++ "'"]
    errCont (Conflicting _ n) = ["conflicting definitions for '" ++ n ++ "'"]

data ElabWarning
    = Unreachable SourcePos [ClauseRow]

instance RenderableError ElabWarning where
    errPos (Unreachable p _) = p

    errType _ = "source"

    errCont (Unreachable _ rows) = ["some rows are unreachable"]

instance Show ElabWarning where
    show (Unreachable p rows) = "Unreachable " ++ show p ++ show (fmap (\(a,_,_) -> a) rows)

type Action = Map.Map Name Name -> Elaborator (Core SourcePos)
type ClauseRow = ([SourcePattern], Map.Map Name Name, Action)
type ClauseMatrix = ([ClauseRow], [Name])

type ElabEnv = (Module, Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier Scheme)
type ElabState = Int
type Elaborator = ErrorsT [ElabError] (RWS ElabEnv [ElabWarning] ElabState)

fresh :: Elaborator Name
fresh = do
    n <- get
    put (n+1)
    pure ('v':show n)

fork :: Name -> Elaborator Name
fork i = do
    n <- get
    put (n+1)
    pure (i ++ "_" ++ show n)