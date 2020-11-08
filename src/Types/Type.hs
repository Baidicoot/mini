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
    deriving(Eq, Ord)

intty :: Type
intty = Node NoTag (Builtin IntTy)

unitty :: Type
unitty = Node NoTag (Builtin UnitTy)

litTy :: UnboxedLit -> Type
litTy = Node NoTag . Builtin . litPrimTy

opTy :: Primop -> Type
opTy AAdd = intty --> intty --> intty
opTy ASub = intty --> intty --> intty
opTy ADiv = intty --> intty --> intty
opTy AMul = intty --> intty --> intty

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
    show (App _ a b) = show a ++ " " ++ showPar b
    show (Node _ t) = show t

data PolyScheme t = Forall (Set.Set Name) (TaggedAppGraph t TypeNode) deriving(Eq)
type Scheme = PolyScheme NoTag
type SourceScheme = PolyScheme SourcePos

qualified :: PolyScheme t -> Set.Set Name
qualified (Forall a _) = a

unqualified :: PolyType t -> PolyScheme t
unqualified = Forall mempty

untagScheme :: PolyScheme a -> Scheme
untagScheme (Forall x t) = Forall x (untag t)

instance Show (PolyScheme t) where
    show (Forall ns t) = "∀" ++ unwords (Set.toList ns) ++ ". " ++ show t

type Subst = Map.Map Name Type

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Name

instance Substitutable Subst where
    apply s = fmap (apply s)
    ftv = foldr Set.union Set.empty . fmap ftv

instance {-# OVERLAPPING #-} Substitutable Type where
    apply s = join . fmap (applyN s)
        where
            applyN s t@(TypeVar n) = Map.findWithDefault (Node NoTag t) n s
            applyN _ t = (Node NoTag t)
    ftv = foldr mappend mempty . fmap ftvN
        where
            ftvN (TypeVar n) = Set.singleton n
            ftvN _ = Set.empty

instance (Substitutable s, Substitutable a) => Substitutable (TaggedAppGraph s a) where
    apply s (App t a b) = App (apply s t) (apply s a) (apply s b)
    apply s (Node t a) = Node (apply s t) (apply s a)
    
    ftv (App t a b) = ftv t `mappend` ftv a `mappend` ftv b
    ftv (Node t a) = ftv t `mappend` ftv a

instance Substitutable NoTag where
    apply _ _ = NoTag
    ftv _ = mempty

data UnifyError
    = OccursUE Name Type
    | MatchUE Type Scheme
    | UnifyUE Type Type
    | RigidUE Name Type
    deriving(Eq)

instance Show UnifyError where
    show (OccursUE n t) = "the metavariable '" ++ n ++ "' occurs in '" ++ show t ++ "'"
    show (MatchUE t s) = "could not match '" ++ show t ++ "' with '" ++ show s ++ "'"
    show (UnifyUE t1 t2) = "could not unify '" ++ show t1 ++ "' with '" ++ show t2 ++ "'"
    show (RigidUE n t) = "could not match '" ++ show t ++ "' with the rigid variable '" ++ n ++ "'"

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
a @@ b = Map.fromList [(u, apply a t) | (u, t) <- Map.toList b] `mappend` a

mapsTo :: Name -> Type -> Subst
mapsTo n t = Map.singleton n t

varBind :: Name -> Type -> Either UnifyError Subst
varBind u t
    | t == Node NoTag (TypeVar u) = Right mempty
    | u `Set.member` ftv t = Left (OccursUE u t)
    | otherwise = Right (u `mapsTo` t)

mgu :: Type -> Type -> Either UnifyError Subst
mgu (App _ a b) (App _ c d) = do
    s1 <- mgu a c
    s2 <- mgu (apply s1 b) (apply s1 d)
    Right (s1 @@ s2)
mgu (Node _ (TypeVar u)) t = varBind u t
mgu t (Node _ (TypeVar u)) = varBind u t
mgu a b
    | a == b = Right mempty
mgu a b = Left (UnifyUE a b)

match :: Type -> Scheme -> Either UnifyError Subst
match (App _ w x) (Forall a (App _ y z)) = do
    s1 <- match w (Forall a y)
    s2 <- match (apply s1 x) (Forall a $ apply s1 z)
    Right (s1 @@ s2)
match (Node _ (TypeVar u)) (Forall a t) = varBind u t
match t (Forall a (Node _ (TypeVar u)))
    | not (u `Set.member` a) = varBind u t
match x (Forall a y)
    | x == y = Right mempty
match a b = Left (MatchUE a b)

infixr 9 -->
(-->) :: Type -> Type -> Type
a --> b = App NoTag (App NoTag (Node NoTag FunctionType) a) b

arity :: PolyType t -> Int
arity (App _ (App _ (Node _ FunctionType) _) b) = 1 + arity b
arity _ = 0

aritySc :: PolyScheme t -> Int
aritySc (Forall _ t) = arity t

resTy :: Type -> Type
resTy (App _ (App _ (Node _ FunctionType) a) b) = resTy b
resTy x = x

argTys :: Type -> [Type]
argTys (App _ (App _ (Node _ FunctionType) a) b) = a:argTys b
argTys _ = []

instance Substitutable Scheme where
    apply s (Forall ns t) = Forall ns (apply (foldr (\n s -> Map.delete n s) s ns) t)
    ftv (Forall ns t) = ftv t `Set.difference` ns

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable a => Substitutable (Map.Map n a) where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty