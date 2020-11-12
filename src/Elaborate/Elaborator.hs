module Elaborate.Elaborator where

import Types.Pattern
import Types.Core
import Types.Ident
import Types.Type

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

instance Show ElabWarning where
    show (Unreachable p rows) = "Unreachable " ++ show p ++ show (fmap (\(a,_,_) -> a) rows)
    show (Incomplete p) = "Incomplete " ++ show p

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