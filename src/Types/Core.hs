{-# LANGUAGE MultiParamTypeClasses #-}

module Types.IR where

import Types.Ident
import Types.Graph
import Types.Pattern
import Types.Type
import Types.Pretty
import Types.Prim
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Arrow

data Value
    = Var Identifier
    | Lit UnboxedLit
    deriving(Eq, Ord)

instance Show Value where
    show (Var i) = show i
    show (Lit l) = show l

data PatternBinding
    = LiteralPattern UnboxedLit
    | ConsPattern Identifier [Name]
    | WildcardPattern
    deriving(Eq, Ord)

instance Show PatternBinding where
    show (ConsPattern id ns) = "(" ++ show id ++ concatMap (' ':) ns ++ ")"
    show (LiteralPattern l) = show l
    show WildcardPattern = "_"

data CoreNode tag
    = Let Name (Core tag) (Core tag)
    | Fix [(Identifier, Core tag)] (Core tag)
    | Lam Name (Core tag)
    | Val Value
    | Match Name [(PatternBinding, tag, Core tag)]
    | Annot (Core tag) Scheme
    | Cons Identifier [Value]
    | Prim Primop [Value]
    | Error String
    deriving(Eq, Ord)

type Core tag = TaggedAppGraph tag (CoreNode tag)

aconv :: Map.Map Name Name -> Core t -> Core t
aconv m (Val (Var (LocalIdentifier n))) = Val . Var . LocalIdentifier $ Map.findWithDefault n n m
aconv m (Let n a b) = Let n (aconv m a) (aconv m b)
aconv m (Fix f e) = Fix (map (second (aconv m)) f) (aconv m e)
aconv m (Lam n e) = Lam n (aconv m e)
aconv m (Match n p) = Match n (map (\(a,b,c) -> (a,b,aconv m c)) p)
aconv m (Annot a t) = Annot (aconv m a) t
aconv _ x = x

instance Show (CoreNode tag) where
    show (Let n ds ir) = "let " ++ n ++ " = " ++ show ds ++ " in\n" ++ show ir
    show (Fix ds ir) = "fix " ++ intercalate "\n    " (fmap (\(n, ir) -> show n ++ " = " ++ show ir) ds) ++ " in\n" ++ show ir
    show (Cons id args) = "{" ++ show id ++ concatMap (',':) args ++ "}"
    show (Annot ir ty) = "(" ++ show ir ++ " :: " ++ show ty ++ ")"
    show (Lam n ir) = "(\\" ++ n ++ ". " ++ show ir ++ ")"
    show (Val v) = show v
    show (Match ir cases) = "match " ++ ir ++ " with\n" ++ concatMap (\(p, _, ir) -> "    " ++ show p ++ " -> " ++ show ir ++ "\n") cases

instance Pretty (CoreNode tag) Int where
    pretty (Let v ds ir) n = "\n" ++ replicate n ' ' ++ "let " ++ v ++ " = " ++ pretty ds (n+4) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix ds ir) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> show v ++ " = " ++ pretty ir (n+8)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Annot ir ty) n = "(" ++ pretty ir n ++ " :: " ++ show ty ++ ")"
    pretty (Lam v ir) n = "(\\" ++ v ++ ". " ++ pretty ir n ++ ")"
    pretty (Val i) _ = show i
    pretty (Cons id args) _ = "{" ++ show id ++ concatMap (',':) args ++ "}"
    pretty (Match ir cases) n = "match " ++ ir ++ " with\n" ++ intercalate "\n" (fmap (\(p, _, ir) -> replicate n ' ' ++ show p ++ " -> " ++ pretty ir (n+4)) cases)

instance (Substitutable tag) => Substitutable (CoreNode tag) where
    apply s (Fix ds ir) = Fix (fmap (\(a, b) -> (a, apply s b)) ds) (apply s ir)
    apply s (Let n ds ir) = Let n (apply s ds) (apply s ir)
    apply s (Annot ir ty) = Annot (apply s ir) (apply s ty)
    apply s (Lam n ir) = Lam n (apply s ir)
    apply s (Match v cases) = Match v (fmap (\(a, t, b) -> (a, apply s t, apply s b)) cases)
    apply _ x = x

    ftv (Fix ds ir) = mconcat (fmap (\(_, b) -> ftv b) ds) `mappend` ftv ir
    ftv (Let n ds ir) = ftv ds `mappend` ftv ir
    ftv (Annot ir ty) = ftv ir `mappend` ftv ty
    ftv (Lam _ ir) = ftv ir
    ftv (Match v cases) = mconcat (fmap (\(_, _, b) -> ftv b) cases) `mappend` (Set.singleton v)
    ftv _ = mempty