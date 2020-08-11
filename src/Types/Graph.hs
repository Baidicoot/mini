{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Graph where

import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Traversable

data TaggedAppGraph t a
    = App t (TaggedAppGraph t a) (TaggedAppGraph t a)
    | Node t a
    deriving(Eq)

type AppGraph = TaggedAppGraph ()

instance (Show t, Show a) => Show (TaggedAppGraph t a) where
    show (App _ a b) = "(" ++ show a ++ ") (" ++ show b ++ ")"
    show (Node t a) = show a ++ " :: " ++ show t

instance {-# OVERLAPPING #-} Show a => Show (TaggedAppGraph () a) where
    show (App _ a (Node _ b)) = show a ++ " " ++ show b
    show (App _ a b) = show a ++ " (" ++ show b ++ ")"
    show (Node _ a) = show a

instance Functor (TaggedAppGraph t) where
    fmap f (App t a b) = App t (fmap f a) (fmap f b)
    fmap f (Node t a) = Node t (f a)

-- synonym for `join`
joinApp :: TaggedAppGraph t (TaggedAppGraph t a) -> TaggedAppGraph t a
joinApp (App t a b) = App t (joinApp a) (joinApp b)
joinApp (Node _ a) = a

instance Monoid t => Monad (TaggedAppGraph t) where
    a >>= f = joinApp (fmap f a)

instance Monoid t => Applicative (TaggedAppGraph t) where
    pure = Node mempty
    fs <*> as = do
        f <- fs
        a <- as
        pure (f a)

instance Foldable (TaggedAppGraph t) where
    foldMap f (Node _ x) = f x
    foldMap f (App _ a b) = (foldMap f a) `mappend` (foldMap f b)

instance Traversable (TaggedAppGraph t) where
    traverse f (Node t x) = fmap (Node t) (f x)
    traverse f (App t a b) = liftA2 (App t) (traverse f a) (traverse f b) 