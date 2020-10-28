{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Type where

import Types.Ident
import Types.Pattern
import Types.Graph
import Types.Prim

import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Parsec.Pos

data TypeNode
    = FunctionType
    | KindStar
    | Builtin LitType
    | NamedType Identifier
    | TypeVar Name
    deriving(Eq)

intty :: Type
intty = Node NoTag (Builtin IntTy)

unitty :: Type
unitty = Node NoTag (Builtin UnitTy)

type SourceType = SourceGraph TypeNode

type Type = AppGraph TypeNode

type PolyType t = TaggedAppGraph t TypeNode

type Kind = Type

instance Show TypeNode where
    show FunctionType = "(⟶)"
    show KindStar = "★"
    show (NamedType s) = show s
    show (TypeVar s) = s
    show (Builtin t) = show t

showPar :: PolyType t -> String
showPar (Node _ t) = show t
showPar t = "(" ++ show t ++ ")"

showArg :: PolyType t -> String
showArg (App _ (App _ (Node _ FunctionType) i) o) = "(" ++ showArg i ++ " ⟶ " ++ show o ++ ")"
showArg t = show t

instance {-# OVERLAPPING #-} Show (PolyType t) where
    show (App _ (App _ (Node _ FunctionType) i) o) = showArg i ++ " ⟶ " ++ show o
    show (App _ a b) = show a ++ showPar b
    show (Node _ t) = show t

data PolyScheme t = Forall (Set.Set Name) (TaggedAppGraph t TypeNode) deriving(Eq)
type Scheme = PolyScheme NoTag
type SourceScheme = PolyScheme SourcePos

untagScheme :: PolyScheme a -> Scheme
untagScheme (Forall x t) = Forall x (untag t)

instance Show (PolyScheme t) where
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

instance Substitutable NoTag where
    apply _ _ = NoTag
    ftv _ = mempty

applyN :: Subst -> TypeNode -> Type
applyN s t@(TypeVar n) = Map.findWithDefault (Node NoTag t) n s
applyN _ t = (Node NoTag t)

ftvN :: TypeNode -> Set.Set Name
ftvN (TypeVar n) = Set.singleton n
ftvN _ = Set.empty

infixr 9 -->
(-->) :: Type -> Type -> Type
a --> b = App NoTag (App NoTag (Node NoTag FunctionType) a) b

fnTag :: t ->  TaggedAppGraph t TypeNode -> TaggedAppGraph t TypeNode -> TaggedAppGraph t TypeNode
fnTag t a b = App t (App t (Node t FunctionType) a) b

arity :: PolyType t -> Int
arity (App _ (App _ (Node _ FunctionType) _) b) = 1 + arity b
arity _ = 0

zipArgs :: PolyType t -> [Name] -> Maybe ([(Name, PolyType t)], PolyType t)
zipArgs t ns
    | arity t == length ns = Just $ internal t ns
    where
        internal (App _ (App _ (Node _ FunctionType) a) b) (x:xs) = let (rs, r) = internal b xs in
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

generalize :: Monomorphic -> PolyType t -> PolyScheme t
generalize mono t = Forall as t
    where as = ftv (untag t) `Set.difference` mono