{-# LANGUAGE FunctionalDependencies #-}
module Checker.Types where

import Types.Kind
import Types.Type
import Types.Ident
import Types.Pattern
import Parser.Syntax
import qualified Checker.Kinds as Kinds

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

