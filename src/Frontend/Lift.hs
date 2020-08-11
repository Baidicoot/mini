module Frontend.Lift where

import Types.Graph
import Types.Type
import Types.Pattern
import Types.Ident
import Types.IR

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Lifter = 