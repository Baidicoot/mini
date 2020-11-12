module Elaborate.Elaborator where

import Types.Pattern
import Types.Core
import Types.Ident

import Control.Monad.Errors
import Control.Monad
import Control.Monad.RWS

import Text.Parsec.Pos

import qualified Data.Map as Map

data ElabError
    = UnboundTerm SourcePos Identifier
    | UnboundType SourcePos Identifier
    | Conflicting SourcePos Name
    deriving(Show)

data ElabWarning
    = Unreachable SourcePos [ClauseRow]
    | Incomplete SourcePos
    deriving(Show)

type Action = Map.Map Name Name -> Elaborator (Core SourcePos)
type ClauseRow = ([SourcePattern], Map.Map Name Name, Action)
type ClauseMatrix = ([ClauseRow], [Name])

type ElabEnv = (Module, Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier Scheme)
type ElabState = Int
type Elaborator = ErrorsT [ElabError] (RWS ElabEnv [ElabWarning] ElabState)