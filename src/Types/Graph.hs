{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Graph where

import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Traversable

import Types.Pretty

data TaggedAppGraph t a
    = App t (TaggedAppGraph t a) (TaggedAppGraph t a)
    | Node t a
    deriving(Eq)

type AppGraph = TaggedAppGraph ()

instance {-# OVERLAPPING #-} Show a => Show (TaggedAppGraph () a) where
    show (App _ a (Node _ b)) = show a ++ " " ++ show b
    show (App _ a b) = show a ++ " (" ++ show b ++ ")"
    show (Node _ a) = show a

instance (Show t, Show a) => Show (TaggedAppGraph t a) where
    show (App _ a b) = "(" ++ show a ++ ") (" ++ show b ++ ")"
    show (Node t a) = show a ++ " :: " ++ show t

instance {-# OVERLAPPING #-} (Pretty a d) => Pretty (TaggedAppGraph () a) d where
    pretty (App _ a (Node _ b)) d = pretty a d ++ " " ++ pretty b d
    pretty (App _ a b) d = pretty a d ++ " (" ++ pretty b d ++ ")"
    pretty (Node _ a) d = pretty a d

instance (Pretty a d, Show t) => Pretty (TaggedAppGraph t a) d where
    pretty (App _ a (Node _ b)) d = pretty a d ++ " " ++ pretty b d
    pretty (App _ a b) d = pretty a d ++ " (" ++ pretty b d ++ ")"
    pretty (Node t a) d = pretty a d

instance Functor (TaggedAppGraph t) where
    fmap f (App t a b) = App t (fmap f a) (fmap f b)
    fmap f (Node t a) = Node t (f a)

-- synonym for `join`
joinApp :: TaggedAppGraph t (TaggedAppGraph t a) -> TaggedAppGraph t a
joinApp (App t a b) = App t (joinApp a) (joinApp b)
joinApp (Node _ a) = a

traverseWithTag :: Applicative f => (n -> t -> f (TaggedAppGraph t n)) -> TaggedAppGraph t n -> f (TaggedAppGraph t n)
traverseWithTag f (App t a b) = liftA2 (App t) (traverseWithTag f a) (traverseWithTag f b)
traverseWithTag f (Node t n) = f n t

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