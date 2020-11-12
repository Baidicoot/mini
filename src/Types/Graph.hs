{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Graph where

import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos

import Types.Pretty

data TaggedAppGraph t a
    = App t (TaggedAppGraph t a) (TaggedAppGraph t a)
    | Node t a
    deriving(Eq, Ord)

getTag :: TaggedAppGraph t a -> t
getTag (App t _ _) = t
getTag (Node t _) = t

data NoTag = NoTag deriving(Eq, Ord)

instance Show NoTag where
    show _ = ""

instance Semigroup NoTag where
    _ <> _ = NoTag

instance Monoid NoTag where
    mempty = NoTag
    mappend = (<>)

type AppGraph = TaggedAppGraph NoTag
type SourceGraph = TaggedAppGraph SourcePos

isNode :: TaggedAppGraph t a -> Bool
isNode (Node _ _) = True
isNode _ = False

showpar :: (Show a) => TaggedAppGraph t a -> String
showpar a
    | isNode a = show a
    | otherwise = "(" ++ show a ++ ")"

prettypar :: (Pretty a d, Show t) => TaggedAppGraph t a -> d -> String
prettypar a d
    | isNode a = pretty a d
    | otherwise = "(" ++ pretty a d ++ ")"

instance (Show a) => Show (TaggedAppGraph t a) where
    show (App _ a b) = show a ++ " " ++ showpar b
    show (Node t a) = show a

instance (Pretty a d, Show t) => Pretty (TaggedAppGraph t a) d where
    showtag (Node _ a) d = showtag a d
    showtag (App _ a b) d = showtag a d && showtag b d

    pretty (App _ a b) d = pretty a d ++ " " ++ prettypar b d
    pretty (Node t a) d
        | showtag a d = pretty a d ++ " :: " ++ show t
        | otherwise = pretty a d

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

untag :: TaggedAppGraph t a -> AppGraph a
untag (App _ a b) = App NoTag (untag a) (untag b)
untag (Node _ x) = Node NoTag x