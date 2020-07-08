{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_mini (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/aidan/.cabal/bin"
libdir     = "/home/aidan/.cabal/lib/x86_64-linux-ghc-8.0.2/mini-0.1.0.0"
dynlibdir  = "/home/aidan/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/aidan/.cabal/share/x86_64-linux-ghc-8.0.2/mini-0.1.0.0"
libexecdir = "/home/aidan/.cabal/libexec"
sysconfdir = "/home/aidan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mini_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mini_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mini_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mini_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mini_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mini_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
