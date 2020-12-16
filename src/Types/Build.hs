module Types.Build where

import Types.Module
import Control.Monad.Errors
import Types.Abstract
import Types.Ident

data BuildConfig = BuildConfig { root :: String, backend :: Backend, backendName :: String, flags :: [String] }

data CachedFile = CachedFile { api :: ModuleAPI, abi :: ModuleABI, filepath :: String }

type Build = ErrorsT [String] IO

data Backend = Backend
    { assemble :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
    , registers :: Int
    , mainLabel :: Identifier
    }