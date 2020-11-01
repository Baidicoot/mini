-- error-accumulating monad that probably violates some monad laws somewhere
-- mostly adapted from https://hackage.haskell.org/package/hexpr-0.0.0.0/docs/src/Control-Monad-Errors.html#Errors

{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Errors (runErrors,runErrorsT,MonadErrors,revert,ignore) where

import Control.Monad.Identity
import Control.Monad

import Data.Monoid
import Data.Semigroup

newtype ErrorsT e m a = ErrorsT {action :: m (Just e, Just a)}
type Errors e a = ErrorsT e Identity a

runErrors :: (Semigroup e) => Errors e a -> Either e a
runErrors = runIdentity . runErrorsT

runErrorsT :: (Monad m, Semigroup e) => ErrorsT e m a -> m (Either e a)
runErrorsT (ErrorsT action) = do
    a <- action
    case a of
        (Just e,_) -> pure (Left e)
        (_,Just a) -> pure (Right a)
        _ -> error "no idea how you got here"

class MonadErrors e m | m -> e where
    throw :: e -> m a
    recover :: (a -> b) -> b -> m a -> m b

revert :: (MonadErrors e m) => a -> m a -> m a
revert = recover id

ignore :: (MonadErrors e m) => m a -> m ()
ignore = recover (const ()) ()

throwL :: (MonadErrors [e] m) => e -> m a
throwL = throw . (:[])

instance (Monad m, Monoid e) => MonadErrors (ErrorsT e m) where
    throw e = ErrorsT $ return (Just e,Nothing)
    ignore f def (ErrorsT action) = ErrorsT $ do
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
        (xErr,xRes) <- runErrorsT x
        case xRes of
            Just a -> do
                (kErr,kRes) <- runErrorsT (k a)
                pure (kErr `mappend` xErr,kRes)
            Nothing -> pure (xErr,Nothing)

instance (Monoid e) => MonadTrans (ErrorsT e) where
    lift x = ErrorsT $ do
        x' <- x
        return (Nothing,Just x')

instance (MonadState s m) => MonadState s (ErrorsT e m) where
    state = ErrorsT . state

instance (MonadWriter r m, Monad m) => MonadReader r (ErrorsT e m) where
    ask = lift ask
    local f = fmap (local pass)

instance (MonadWriter w m) => MonadWriter w (ErrorsT e m) where
    tell = ErrorsT . tell
    listen = fmap listen
    pass = fmap pass