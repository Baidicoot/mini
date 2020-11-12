module Types.Injection where

import Data.Maybe (listToMaybe)
import Data.List (any)

newtype Injection a b = Injection [(a,b)]

injectify :: ([(a,b)] -> [(a,b)]) -> Injection a b -> Injection a b
injectify f (Injection ns) = Injection (f ns)

deleteFst :: (Eq a) => a -> Injection a b -> Injection a b
deleteFst a = injectify $ \ns -> filter (\(a',_) -> a'/=a) ns

deleteSnd :: (Eq b) => b -> Injection a b -> Injection a b
deleteSnd b = injectify $ \ns -> filter (\(_,b') -> b'/=b) ns

insert :: (Eq b) => a -> b -> Injection a b -> Injection a b
insert a b = injectify ((a,b):) . deleteSnd b

filterGen :: ((a,b) -> Bool) -> Injection a b -> Injection a b
filterGen f = injectify (filter (\p -> f p))

filterFst :: (a -> Bool) -> Injection a b -> Injection a b
filterFst f = filterGen (\(a,_) -> f a)

filterSnd :: (b -> Bool) -> Injection a b -> Injection a b
filterSnd f = filterGen (\(_,b) -> f b)

lookup :: (Eq a) => a -> Injection a b -> [(a,b)]
lookup a (Injection ns) = filter (\(a',_) -> a==a') ns

lookupInv :: (Eq b) => b -> Injection a b -> Maybe (a,b)
lookupInv b (Injection ns) = listToMaybe $ filter (\(_,b') -> b==b') ns

member :: (Eq a) => a -> Injection a b -> Bool
member a (Injection ns) = any (\(a',_) -> a'==a) ns

memberInv :: (Eq b) => b -> Injection a b -> Bool
memberInv b (Injection ns) = any (\(_,b') -> b'==b) ns

empty :: Injection a b
empty = Injection []

fromList :: (Eq a, Eq b) => [(a,b)] -> Injection a b
fromList ((a,b):xs) = insert a b (fromList xs)
fromList [] = mempty

-- left biased union
union :: (Eq b) => Injection a b -> Injection a b -> Injection a b
union (Injection as) bs = foldr (\(a,b) inj -> insert a b inj) bs as

instance (Eq a, Eq b) => Semigroup  (Injection a b) where
    (<>) = union

instance (Eq a, Eq b) => Monoid (Injection a b) where
    mempty = empty
    mappend = (<>)