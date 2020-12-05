-- error-accumulating monad that probably violates some monad laws somewhere
-- mostly adapted from https://hackage.haskell.org/package/hexpr-0.0.0.0/docs/src/Control-Monad-Errors.html#Errors

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Errors
    ( runErrors
    , runErrorsT
    , MonadErrors
    , revert
    , ignore
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
    , ErrorsResult(..)
    ) where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad

import Data.Monoid
import Data.Semigroup

newtype ErrorsT e m a = ErrorsT {action :: m (Maybe e, Maybe a)}
type Errors e a = ErrorsT e Identity a

second :: (b -> c) -> (a, b) -> (a, c)
second f (a,b) = (a,f b)

data ErrorsResult e a
    = FailWithResult e a
    | Fail e
    | Success a
    deriving(Eq, Show)

mapErr :: (e -> f) -> ErrorsResult e a -> ErrorsResult f a
mapErr f (Fail e) = Fail (f e)
mapErr f (FailWithResult e a) = FailWithResult (f e) a
mapErr _ (Success a) = Success a

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
runErrorsT (ErrorsT action) = do
    a <- action
    case a of
        (Just e,Just a) -> pure (FailWithResult e a)
        (Just e,_) -> pure (Fail e)
        (_,Just a) -> pure (Success a)
        _ -> error "no idea how you got here"

class MonadErrors e m | m -> e where
    throw :: e -> m a
    recover :: (a -> b) -> b -> m a -> m b

revert :: (MonadErrors e m) => a -> m a -> m a
revert = recover id

ignore :: (MonadErrors e m) => m a -> m ()
ignore = recover (const ()) ()

err :: (MonadErrors e m) => a -> e -> m a
err a = revert a . throw

report :: (MonadErrors e m) => e -> m ()
report = ignore . throw

throwL :: (MonadErrors [e] m) => e -> m a
throwL = throw . (:[])

errL :: (MonadErrors [e] m) => a -> e -> m a
errL a e = revert a (throwL e)

instance (Monad m, Monoid e) => MonadErrors e (ErrorsT e m) where
    throw e = ErrorsT $ return (Just e,Nothing)
    recover f def (ErrorsT action) = ErrorsT $ do
        (x,a) <- action
        case a of
            Nothing -> pure (x,Just def)
            Just a' -> pure (x,Just (f a'))

instance (Monad m, Monoid e) => Functor (ErrorsT e m) where
    fmap = liftM

instance (Monad m, Monoid e) => Applicative (ErrorsT e m) where
    pure = return
    (<*>) = ap

instance (Monad m, Monoid e) => Monad (ErrorsT e m) where
    return x = ErrorsT $ return (Nothing,Just x)
    x >>= k = ErrorsT $ do
        (xErr,xRes) <- action x
        case xRes of
            Just a -> do
                (kErr,kRes) <- action (k a)
                pure (kErr `mappend` xErr,kRes)
            Nothing -> pure (xErr,Nothing)

-- Instances for mtl transformers
-- All of these instances need UndecidableInstances,
-- as they do not satisfy the coverage condition.

instance (Monoid e) => MonadTrans (ErrorsT e) where
    lift x = ErrorsT $ do
        x' <- x
        return (Nothing,Just x')

instance (MonadState s m, Monoid e) => MonadState s (ErrorsT e m) where
    state f = lift (state f)

instance (MonadReader r m, Monad m, Monoid e) => MonadReader r (ErrorsT e m) where
    ask = lift ask
    local f = ErrorsT . local f . action

instance (MonadWriter w m, Monad m, Monoid e) => MonadWriter w (ErrorsT e m) where
    tell = lift . tell
    listen a = ErrorsT $ do
        (aErr,aRes) <- action a
        case aRes of
            Just b -> do
                c <- listen (pure b)
                pure (aErr,Just c)
            Nothing -> pure (aErr,Nothing)
    pass a = ErrorsT $ do
        (aErr,aRes) <- action a
        case aRes of
            Just b -> do
                c <- pass (pure b)
                pure (aErr,Just c)
            Nothing -> pure (aErr,Nothing)
