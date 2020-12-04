module Main where

import Glue
import Control.Monad
import Control.Monad.Except (runExceptT, catchError, liftIO)

import Data.IORef
import Types.Env

config :: Config
config = Config {regs=10}

main :: IO ()
main = void
    . runExceptT $ do
        forever $ do
            line <- liftIO $ prompt "> "
            do {
                compileStr config line;
                pure ();
            } `catchError` mapM_ (liftIO . putStrLn)