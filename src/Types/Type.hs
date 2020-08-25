{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Type where

import Types.Ident
import Types.Pattern
import Types.Graph

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

data TypeNode
    = FunctionType
    | KindStar
    | NamedType Identifier
    | TypeVar Name
    deriving(Eq)

type Type = AppGraph TypeNode

type Kind = Type

instance Show TypeNode where
    show FunctionType = "⟶"
    show KindStar = "★"
    show (NamedType s) = show s
    show (TypeVar s) = s

instance {-# OVERLAPPING #-} Show Type where
    show (App () (Node () FunctionType) (Node () x)) = show x ++ " ⟶"
    show (App () (Node () FunctionType) x) = "(" ++ show x ++ ") ⟶"
    show (App () a (Node () b)) = show a ++ " " ++ show b
    show (App () a b) = show a ++ " (" ++ show b ++ ")"
    show (Node () x) = show x

data Scheme = Forall (Set.Set Name) Type deriving(Eq)

instance Show Scheme where
    show (Forall ns t) = "∀" ++ unwords (Set.toList ns) ++ ". " ++ show t

type Monomorphic = Set.Set Name
type Subst = Map.Map Name Type

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Name

instance Substitutable Subst where
    apply s = fmap (apply s)
    ftv = foldr Set.union Set.empty . fmap ftv

instance (Substitutable s, Substitutable a) => Substitutable (TaggedAppGraph s a) where
    apply s (App t a b) = App (apply s t) (apply s a) (apply s b)
    apply s (Node t a) = Node (apply s t) (apply s a)
    
    ftv (App t a b) = ftv t `mappend` ftv a `mappend` ftv b
    ftv (Node t a) = ftv t `mappend` ftv a

instance Substitutable () where
    apply _ _ = ()
    ftv _ = mempty

applyN :: Subst -> TypeNode -> Type
applyN s t@(TypeVar n) = Map.findWithDefault (Node () t) n s
applyN _ t = (Node () t)

ftvN :: TypeNode -> Set.Set Name
ftvN (TypeVar n) = Set.singleton n
ftvN _ = Set.empty

arity :: Type -> Int
arity (App () (App () (Node () FunctionType) _) b) = 1 + arity b
arity _ = 0

zipArgs :: Type -> [Name] -> Maybe ([(Name, Type)], Type)
zipArgs t ns
    | arity t == length ns = Just $ internal t ns
    where
        internal (App () (App () (Node () FunctionType) a) b) (x:xs) = let (rs, r) = internal b xs in
            ((x, a):rs, r)
        internal t [] = ([], t)
zipArgs _ _ = Nothing

instance {-# OVERLAPPING #-} Substitutable Type where -- pretty sure they aren't overlapping?
    apply s = join . fmap (applyN s)
    ftv = foldr mappend mempty . fmap ftvN

instance Substitutable Scheme where
    apply s (Forall ns t) = Forall ns (apply (foldr (\n s -> Map.delete n s) s ns) t)
    ftv (Forall ns t) = ftv t `Set.difference` ns

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv = foldr (Set.union . ftv) Set.empty

generalize :: Monomorphic -> Type -> Scheme
generalize mono t = Forall as t
    where as = ftv t `Set.difference` mono