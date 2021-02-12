{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Type where

import Types.Ident
import Types.Graph
import Types.Prim

import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Parsec.Pos

data TypeNode t
    = FunctionType
    | KindStar
    | Builtin LitType
    | NamedType Identifier
    | Product [PolyType t]
    | TypeVar Name
    deriving(Eq, Ord)

intty :: Type
intty = Node NoTag (Builtin IntTy)

charty :: Type
charty = Node NoTag (Builtin CharTy)

unitty :: Type
unitty = Node NoTag (Product [])

litTy :: UnboxedLit -> Scheme
litTy (Int _) = unqualified intty
litTy (Char _) = unqualified charty

refty :: Type -> Type
refty = App NoTag (Node NoTag (Builtin RefTy))

opTy :: Primop -> Type
opTy AAdd = intty --> intty --> intty
opTy ASub = intty --> intty --> intty
opTy ADiv = intty --> intty --> intty
opTy AMul = intty --> intty --> intty
opTy PutChr = charty --> unitty
opTy PutInt = intty --> unitty
opTy IntToChar = intty --> charty
opTy CharToInt = charty --> intty
opTy CmpInt = intty --> intty --> tv "a" --> tv "a" --> tv "a" --> tv "a"
opTy CmpChar = charty --> charty --> tv "a" --> tv "a" --> tv "a" --> tv "a"
opTy EqInt = intty --> intty --> tv "a" --> tv "a" --> tv "a"
opTy EqChar = charty --> charty --> tv "a" --> tv "a" --> tv "a"
opTy SetRef = refty (tv "a") --> tv "a" --> unitty
opTy NewRef = tv "a" --> refty (tv "a")
opTy GetRef = refty (tv "a") --> tv "a"

type SourceType = SourceGraph (TypeNode SourcePos)

type Type = AppGraph (TypeNode NoTag)

type PolyType t = TaggedAppGraph t (TypeNode t)

type Kind = Type

instance Show (TypeNode t) where
    show FunctionType = "(⟶)"
    show KindStar = "★"
    show (NamedType s) = show s
    show (TypeVar s) = show s
    show (Builtin t) = show t
    show (Product ts) = "{" ++ intercalate "," (fmap show ts) ++ "}"

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

data PolyScheme t = Forall (Set.Set Name) (PolyType t) deriving(Eq)
type Scheme = PolyScheme NoTag
type SourceScheme = PolyScheme SourcePos

qualified :: PolyScheme t -> Set.Set Name
qualified (Forall a _) = a

unqualified :: PolyType t -> PolyScheme t
unqualified = Forall mempty

untagType :: PolyType a -> Type
untagType = untag . fmap untagTypeN
    where
        untagTypeN (Product ts) = Product (fmap untagType ts)
        untagTypeN FunctionType = FunctionType
        untagTypeN KindStar = KindStar
        untagTypeN (NamedType s) = NamedType s
        untagTypeN (TypeVar s) = TypeVar s
        untagTypeN (Builtin t) = Builtin t

untagScheme :: PolyScheme a -> Scheme
untagScheme (Forall x t) = Forall x (untagType t)

instance Show (PolyScheme t) where
    show (Forall ns t) = "∀" ++ unwords (fmap show (Set.toList ns)) ++ ". " ++ show t

type Subst = Map.Map Name Type

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Name

instance Substitutable Subst where
    apply s = fmap (apply s)
    ftv = foldr Set.union Set.empty . fmap ftv

instance {-# OVERLAPPING #-} Substitutable Type where
    apply s = (applyN s =<<)
        where
            applyN s t@(TypeVar n) = Map.findWithDefault (Node NoTag t) n s
            applyN s (Product ts) = Node NoTag (Product (fmap (apply s) ts))
            applyN _ t = Node NoTag t
    ftv = foldr mappend mempty . fmap ftvN
        where
            ftvN :: TypeNode NoTag -> Set.Set Name
            ftvN (TypeVar n) = Set.singleton n
            ftvN (Product ts) = mconcat (fmap ftv ts)
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
    | MatchUE (Set.Set Name) Type Type
    | UnifyUE Type Type
    | RigidUE Name Type
    | ProdUE [Type] [Type]
    deriving(Eq)

instance Show UnifyError where
    show (OccursUE n t) = "the metavariable '" ++ show n ++ "' occurs in '" ++ show t ++ "'"
    show (MatchUE _ t1 t2) = "could not match '" ++ show t1 ++ "' with '" ++ show t2 ++ "'"
    show (UnifyUE t1 t2) = "could not unify '" ++ show t1 ++ "' with '" ++ show t2 ++ "'"
    show (RigidUE n t) = "could not match '" ++ show t ++ "' with the rigid variable '" ++ show n ++ "'"
    show (ProdUE as bs) = "could not match the variables " ++ show as ++ "with the variables " ++ show bs

data Eqtn = Eqtn Type Type

instance Show Eqtn where
    show (Eqtn i o) = "(eqtn (" ++ show i ++ ") " ++ show o ++ ")"

validCoercion :: Set.Set Identifier -> Set.Set Name -> Type -> Type -> Bool
validCoercion c q i o = go (leftmost i) && go (leftmost o)
    where
        go (NamedType n) = n `Set.member` c
        go (TypeVar n) = n `Set.member` q
        go _ = False

appEqtn :: Eqtn -> Set.Set Identifier -> Set.Set Name -> Type -> Maybe (Type,Subst)
appEqtn (Eqtn i o) c q t = case match' [] Set.empty q i t of
    Right s ->
        let o' = apply s o
            i' = apply s i
            s' = Map.filterWithKey (\k _ -> k `Set.member` ftv t) s
        in if validCoercion c q i' o' then Just (o',s') else Nothing
    Left _ -> Nothing

match :: [Eqtn] -> Set.Set Identifier -> Set.Set Name -> Type -> Type -> Either UnifyError Subst
match e c q a b = case match' e c q a b of
    Right s -> Right s
    Left er -> searchReductions er e e
    where
        searchReductions er (eq:ea) eb = case appEqtn eq c q a of
            Just (a',s1) -> case match e c q a' (apply s1 b) of
                Left _ -> searchReductions er ea eb
                Right s2 -> Right (s1 @@ s2)
            Nothing -> searchReductions er ea eb
        searchReductions er ea (eq:eb) = case appEqtn eq c q b of
            Just (b',s1) -> case match e c q (apply s1 a) b' of
                Left _ -> searchReductions er ea eb
                Right s2 -> Right (s1 @@ s2)
            Nothing -> searchReductions er ea eb
        searchReductions er [] [] = Left er

matchable :: Set.Set Identifier -> Type -> Bool
matchable c t = case leftmost t of
    NamedType n -> not (n `Set.member` c)
    Builtin t -> matchableLit t
    _ -> False

toMatchable :: [Eqtn] -> Set.Set Identifier -> Set.Set Name -> Type -> Maybe Type
toMatchable _ c _ t | matchable c t = pure t
toMatchable e c q t = searchReductions e t
    where
        searchReductions (eq:e) t = case appEqtn eq c q t of
            Just (t,_) -> case toMatchable e c q t of
                Nothing -> searchReductions e t
                Just t' -> pure t'
            Nothing -> searchReductions e t
        searchReductions [] _ = Nothing

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
a @@ b = Map.fromList [(u, apply a t) | (u, t) <- Map.toList b] `mappend` a

mapsTo :: Name -> Type -> Subst
mapsTo = Map.singleton

varBind :: Name -> Type -> Either UnifyError Subst
varBind u t
    | t == Node NoTag (TypeVar u) = Right mempty
    | u `Set.member` ftv t = Left (OccursUE u t)
    | otherwise = Right (u `mapsTo` t)

mgu :: [Eqtn] -> Set.Set Identifier -> Type -> Type -> Either UnifyError Subst
mgu eq c = match eq c Set.empty

matchMany :: [Eqtn] -> Set.Set Identifier -> Set.Set Name -> [Type] -> [Type] -> Either UnifyError Subst
matchMany e c q _ [] = Right mempty
matchMany e c q (a:as) (b:bs) = do
    s1 <- match e c q a b
    s2 <- matchMany e c q (fmap (apply s1) as) (fmap (apply s1) bs)
    Right (s1 @@ s2)
matchMany e c q as bs = Left (ProdUE as bs)

match' :: [Eqtn] -> Set.Set Identifier -> Set.Set Name -> Type -> Type -> Either UnifyError Subst
match' e c q (App _ w x) (App _ y z) = do
    s1 <- match e c q w y
    s2 <- match e c q (apply s1 x) (apply s1 z)
    Right (s1 @@ s2)
match' e c q (Node _ (Product as)) (Node _ (Product bs)) = matchMany e c q as bs
match' e c q x y
    | x == y = Right mempty
match' e c q (Node _ (TypeVar m)) (Node _ (TypeVar n))
    | not (m `Set.member` q) = varBind m (Node NoTag (TypeVar n))
    | not (n `Set.member` q) = varBind n (Node NoTag (TypeVar m))
    | otherwise = Left (RigidUE m (Node NoTag (TypeVar n)))
match' e c q (Node _ (TypeVar u)) t
    | not (u `Set.member` q) = varBind u t
    | otherwise = Left (RigidUE u t)
match' e c q t (Node _ (TypeVar u))
    | not (u `Set.member` q) = varBind u t
    | otherwise = Left (RigidUE u t)
match' e c q a b = Left (MatchUE q a b)

constructor :: Type -> Maybe Identifier
constructor (App _ a _) = constructor a
constructor (Node _ (NamedType i)) = Just i
constructor _ = Nothing

leftmost :: Type -> TypeNode NoTag
leftmost (App _ a _) = leftmost a
leftmost (Node _ n) = n

infixr 9 -->
(-->) :: Type -> Type -> Type
a --> b = App NoTag (App NoTag (Node NoTag FunctionType) a) b

tv :: String -> Type
tv = Node NoTag . TypeVar . User

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
    apply s (Forall ns t) = Forall ns (apply (foldr Map.delete s ns) t)
    ftv (Forall ns t) = ftv t `Set.difference` ns

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable a => Substitutable (Map.Map n a) where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty