{-# LANGUAGE LambdaCase #-}

module Build.Compile where

import Types.Build
import Modules.Glue
import Modules.Module

import Parser.Parser
import Types.Ident
import Types.Module
import Types.Abstract

import Control.Monad.Errors
import Control.Monad
import Control.Monad.IO.Class

import Data.List

compile :: Int -> Int -> BuildConfig -> ModuleServer -> [(ModulePath,Either CachedFile (ParseResult,Stream))] -> [(ModulePath,Either CachedFile [Operator])] -> Build String
compile index num cfg ms ((p,Right (pr,s)):fs) done = do
    liftIO . putStrLn $ "compiling " ++ intercalate "." p ++ "... (" ++ show index ++ " of " ++ show num ++ ")"
    (w,api,abi,ops) <- liftEither $ parsedToAbst p ms [] (mainFn p) (registers $ backend cfg) s pr
    liftIO $ mapM_ putStrLn w
    compile (index+1) num cfg (loadModule abi api ms) fs ((p,Right ops):done)
compile index num cfg ms ((p,Left c):fs) done = compile (index+1) num cfg (loadModule (abi c) (api c) ms) fs ((p,Left c):done)
compile _ _ cfg ms [] done = do
    g <- liftEither . mapLeft (fmap show) $ glue (mainLabel $ backend cfg) (registers $ backend cfg) ms
    assemble (backend cfg) cfg (reverse done) g