-- error-accumulating monad that probably violates some monad laws somewhere
-- mostly adapted from https://hackage.haskell.org/package/hexpr-0.0.0.0/docs/src/Control-Monad-Errors.html#Errors

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Errors
    ( runErrors
    , runErrorsT
    , MonadErrors(..)
    , ErrorsT
    , Errors
    , err
    , errL
    , throw
    , throwL
    , report
    , toEither
    , toErrors
    , mapErr
    , mapLeft
    , liftEither
    , ErrorsResult(..)
    ) where

import Control.Monad.Identity
import Control.Monad.Trans.Class
--import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad

import Data.Monoid
import Data.Semigroup

newtype ErrorsT e m a = ErrorsT {action :: m (ErrorsResult e a)}
type Errors e a = ErrorsT e Identity a

second :: (b -> c) -> (a, b) -> (a, c)
second f (a,b) = (a,f b)

data ErrorsResult e a
    = FailWithResult e a
    | Fail e
    | Success a
    deriving(Eq, Show)

addErr :: Monoid e => e -> ErrorsResult e a -> ErrorsResult e a
addErr e (Success a) = FailWithResult e a
addErr e (Fail f) = Fail (e `mappend` f)
addErr e (FailWithResult f a) = FailWithResult (e `mappend` f) a

mapErr :: (e -> f) -> ErrorsResult e a -> ErrorsResult f a
mapErr f (Fail e) = Fail (f e)
mapErr f (FailWithResult e a) = FailWithResult (f e) a
mapErr _ (Success a) = Success a

instance Functor (ErrorsResult e) where
    fmap f (Success a) = Success (f a)
    fmap f (FailWithResult e a) = FailWithResult e (f a)
    fmap f (Fail e) = Fail e

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

toEither :: ErrorsResult e a -> Either e a
toEither (FailWithResult e _) = Left e
toEither (Fail e) = Left e
toEither (Success a) = Right a

toErrors :: Either e a -> ErrorsResult e a
toErrors (Left e) = Fail e
toErrors (Right a) = Success a

runErrors :: (Monoid e) => Errors e a -> ErrorsResult e a
runErrors = runIdentity . runErrorsT

runErrorsT :: (Monad m, Monoid e) => ErrorsT e m a -> m (ErrorsResult e a)
runErrorsT  = action

class MonadErrors e m | m -> e where
    liftErrors :: ErrorsResult e a -> m a

throw :: (MonadErrors e m) => e -> m a
throw e = liftErrors (Fail e)

err :: (MonadErrors e m) => a -> e -> m a
err a e = liftErrors (FailWithResult e a)

report :: (MonadErrors e m) => e -> m ()
report e = liftErrors (FailWithResult e ())

throwL :: (MonadErrors [e] m) => e -> m a
throwL = throw . (:[])

errL :: (MonadErrors [e] m) => a -> e -> m a
errL a = err a . (:[])

liftEither :: (MonadErrors e m) => Either e a -> m a
liftEither = liftErrors . toErrors

instance (Monad m, Monoid e) => MonadErrors e (ErrorsT e m) where
    liftErrors = ErrorsT . pure

instance (Monad m, Monoid e) => Functor (ErrorsT e m) where
    fmap = liftM

instance (Monad m, Monoid e) => Applicative (ErrorsT e m) where
    pure = return
    (<*>) = ap

instance (Monad m, Monoid e) => Monad (ErrorsT e m) where
    return x = ErrorsT $ return (Success x)
    x >>= k = ErrorsT $ do
        xRes <- action x
        case xRes of
            Success a -> action (k a)
            FailWithResult e a -> addErr e <$> action (k a)
            Fail e -> pure (Fail e)

-- Instances for mtl transformers
-- All of these instances need UndecidableInstances,
-- as they do not satisfy the coverage condition.

instance (Monoid e) => MonadTrans (ErrorsT e) where
    lift x = ErrorsT $ Success <$> x

instance (MonadIO m, Monoid e) => MonadIO (ErrorsT e m) where
    liftIO = lift . liftIO

instance (MonadState s m, Monoid e) => MonadState s (ErrorsT e m) where
    state f = lift (state f)

instance (MonadReader r m, Monad m, Monoid e) => MonadReader r (ErrorsT e m) where
    ask = lift ask
    local f = ErrorsT . local f . action

instance (MonadWriter w m, Monad m, Monoid e) => MonadWriter w (ErrorsT e m) where
    tell = lift . tell
    listen a = ErrorsT $ fmap (\(e, w) -> fmap (, w) e) (listen (action a))
    pass a = ErrorsT $ do
        aRes <- action a
        case aRes of
            Success b -> do
                c <- pass (pure b)
                pure (Success c)
            FailWithResult e b -> do
                c <- pass (pure b)
                pure (FailWithResult e c)
            Fail e -> pure (Fail e)

instance (Monad m, MonadErrors e m, Monoid e) => MonadErrors e (StateT s m) where
    liftErrors = lift . liftErrors

instance (Monad m, MonadErrors e m, Monoid e) => MonadErrors e (ReaderT r m) where
    liftErrors = lift . liftErrors