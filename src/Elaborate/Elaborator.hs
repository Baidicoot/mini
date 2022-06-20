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
    | PatternVarArgs SourcePos Identifier [SourcePattern]
    deriving(Show)

instance RenderableError ElabError where
    errPos (UnboundTerm p _) = p
    errPos (UnboundType p _) = p
    errPos (PatternVarArgs p _ _) = p

    errType _ = "source error"

    errCont (UnboundTerm _ i) = ["unbound identifier '" ++ show i ++ "'"]
    errCont (UnboundType _ i) = ["unbound named type '" ++ show i ++ "'"]
    errCont (PatternVarArgs _ n xs) = ["pattern variable '" ++ show n ++ "' has arguments '" ++ unwords (fmap show xs) ++ "'"]

data ElabWarning
    = Unreachable SourcePos [ClauseRow]

instance RenderableError ElabWarning where
    errPos (Unreachable p _) = p

    errType _ = "source warning"

    errCont (Unreachable _ rows) = ["some rows are unreachable"]

instance Show ElabWarning where
    show (Unreachable p rows) = "Unreachable " ++ show p ++ show (fmap (\(a,_,_) -> a) rows)

type Action = Map.Map Identifier Identifier -> Elaborator (Core SourcePos)
type ClauseRow = ([SourcePattern], Map.Map Identifier Identifier, Action)
type ClauseMatrix = ([ClauseRow], [Identifier])

type ElabEnv = (ModulePath, Map.Map Identifier Identifier, Map.Map Identifier Identifier, Map.Map Identifier Scheme, Map.Map Identifier Identifier)
type ElabState = Int
type Elaborator = ErrorsT [ElabError] (RWS ElabEnv [ElabWarning] ElabState)

fresh :: Elaborator Name
fresh = do
    n <- get
    put (n+1)
    pure (Gen "val" n)

fork :: Name -> Elaborator Name
fork x = case x of
    Gen i _ -> go i
    User i -> go i
    Symb i -> go i
    where
        go :: String -> Elaborator Name
        go i = do
            n <- get
            put (n+1)
            pure (Gen i n)