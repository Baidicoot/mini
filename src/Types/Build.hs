module Types.Build where

import Types.Module
import Control.Monad.Errors
import Types.Abstract
import Types.Ident

data OptFlags = OptFlags { cps_inline_singles :: Bool }

noopt :: OptFlags
noopt = OptFlags False

allopt :: OptFlags
allopt = OptFlags True

data BuildConfig = BuildConfig { root :: String, backend :: Backend, backendName :: String, flags :: [String], opt :: OptFlags }

data CachedFile = CachedFile { api :: ModuleAPI, abi :: ModuleABI, filepath :: String }

type Build = ErrorsT [String] IO

data Backend = Backend
    { assemble :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
    , registers :: Int
    , mainLabel :: Identifier
    }