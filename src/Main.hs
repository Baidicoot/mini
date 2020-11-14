module Main where

import Glue
import Control.Monad
import Control.Monad.Except (runExceptT, catchError, liftIO)

config :: Config
config = Config {regs=10}

main :: IO ()
main = (fmap (const ()))
    . runExceptT
    . forever $ do
        line <- liftIO $ prompt "> "
        do {
            mod <- compileStr config line;
            liftIO $ print mod;
        } `catchError` mapM_ (liftIO . putStrLn)