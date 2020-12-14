module Build.Build where

import Types.Build
import Build.Compile
import Build.Load

import Types.Ident
import Types.Module

build :: BuildConfig -> [ModulePath] -> Build String
build cfg mods = do
    ld <- load (root cfg) mods
    compile 1 (length mods) cfg emptyServer ld []