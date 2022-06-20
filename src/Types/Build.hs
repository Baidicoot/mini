module Types.Build where

import Types.Module
import Control.Monad.Errors
import Types.Abstract
import Types.Ident

data OptFlags = OptFlags
    { cps_inline_singles :: Bool
    , cps_inline_sized :: Bool
    , cps_inline_size :: Int }

cps_size_inline :: Int
cps_size_inline = 0

cps_normal_inline :: Int
cps_normal_inline = 4

cps_max_inline :: Int
cps_max_inline = 8

noopt :: OptFlags
noopt = OptFlags False False 0

allopt :: OptFlags
allopt = OptFlags True True cps_normal_inline

data BuildConfig = BuildConfig { root :: String, backend :: Backend, backendName :: String, flags :: [String], opt :: OptFlags }

data CachedFile = CachedFile { api :: ModuleAPI, abi :: ModuleABI, filepath :: String }

type Build = ErrorsT [String] IO

data Backend = Backend
    { assemble :: BuildConfig -> [(ModulePath,Either CachedFile [Operator])] -> [Operator] -> Build ()
    , registers :: Int
    , mainLabel :: Identifier
    }