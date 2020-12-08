module Build.Load (load) where

-- handles loading input files/from cache & ordering for compilation

import Types.Ident
import Types.Build
import Types.Module

import Parser.Parser

import Control.Monad
import Control.Monad.Errors

import Error.Error

import System.IO
import Control.Monad.IO.Class

import Data.List

getFilePath :: String -> ModulePath -> String
getFilePath s p = s ++ intercalate "/" p ++ ".mi"

getFile :: BuildConfig -> ModulePath -> Build (Either CachedFile (ParseResult,Stream))
getFile (BuildConfig root _ _ _) p = do
    s <- liftIO $ readFile (getFilePath root p)
    case parse p s of
        Right x -> pure (Right (x,s))
        Left e -> err (Right (emptyResult,"")) e

load :: BuildConfig -> [ModulePath] -> Build [(ModulePath,Either CachedFile (ParseResult,Stream))]
load cfg ps = do
    ps' <- forM ps $ \p -> do
        r <- getFile cfg p
        pure (p,either (moduleABIReqs . abi) (fmap fst . fst . fst) r,r)
    liftEither . mapLeft (fmap show) $ sortDependencies ps'