module Types.Bijection where

import Data.Maybe (listToMaybe)

newtype Bijection a b = Bijection [(a,b)]

bijectify :: ([(a,b)] -> [(a,b)]) -> Bijection a b -> Bijection a b
bijectify f (Bijection ns) = Bijection (f ns)

insert :: (Eq a, Eq b) => a -> b -> Bijection a b -> Bijection a b
insert a b = bijectify $ \ns -> (a,b):filter (\(a',b') -> a'/=a && b'/=b) ns

deleteFst :: a -> Bijection a b -> Bijection a b
deleteFst a = bijectify $ \ns -> filter (\(a',_) -> a'/=a) ns

deleteSnd :: b -> Bijection a b -> Bijection a b
deleteSnd b = bijectify $ \ns -> filter (\(_,b') -> b'/=b) ns

lookupFst :: a -> Bijection a b -> Maybe (a,b)
lookupFst a (Bijection ns) = listToMaybe $ filter (\(a',_) -> a==a') ns

lookupSnd :: a -> Bijection a b -> Maybe (a,b)
lookupSnd b (Bijection ns) = listToMaybe $ filter (\(b',_) -> b==b') ns

empty :: Bijection a b
empty = Bijection []

-- left biased union
union :: Bijection a b -> Bijection a b -> Bijection a b
union (Bijection bs) bij = foldr (\(a,b) bij -> insert a b bij) bs bij

instance (Eq a, Eq b) => Monoid (Bijection a b) where
    mempty = empty
    mappend = union