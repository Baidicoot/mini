module Types.Env where

import Types.Ident
import Types.Syntax
import Types.Type
import qualified Data.Map as Map

data Namespace
    = Namespace [Name] [Name] [Name]
    deriving(Eq, Show)

data Typespace
    = Typespace (Map.Map Identifier Scheme) (Map.Map Identifier Scheme)
    deriving(Eq, Show)

data Dataspace
    = Dataspace (Map.Map Identifier (Int, Int, Int))
    -- for every constructor: index, number of constructors, number of arguments
    deriving(Eq, Show)

data ImportAction
    = Include Namespace
    | IncludeHiding Namespace [Name]
    | Import Namespace
    | ImportAs Namespace [Name]
    deriving(Eq, Show)