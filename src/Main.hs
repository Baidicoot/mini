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
        envref <- liftIO $ newIORef mempty
        forever $ do
            line <- liftIO $ prompt "> "
            do {
                env <- liftIO $ readIORef envref;
                liftIO $ print env;
                mod <- compileStr config env line;
                forM_ (termTypes mod) $ \(n,s) -> liftIO . putStrLn $ "Defined " ++ n ++ " : " ++ show s;
                liftIO $ modifyIORef' envref (`mappend` importWithAction include mod);
            } `catchError` mapM_ (liftIO . putStrLn)