{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Core where

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
import Text.Parsec.Pos (SourcePos)

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
    = Let Identifier (Core tag) (Core tag)
    | Fix [(Identifier, Core tag)] (Core tag)
    | Lam Identifier (Core tag)
    | Val Value
    | Match (Maybe Type, SourcePos) Identifier [(PatternBinding, tag, Core tag)]
    | Annot (Core tag) Type
    | Cons Identifier [Value]
    | Tuple [Value]
    | Select Int Value
    | Prim Primop [Value]
    | Error String
    deriving(Eq, Ord)

type Core tag = TaggedAppGraph tag (CoreNode tag)

untagCore :: Core tag -> Core NoTag
untagCore = untag . fmap untagCoreN
    where
        untagCoreN :: CoreNode tag -> CoreNode NoTag
        untagCoreN (Let n a b) = Let n (untagCore a) (untagCore b)
        untagCoreN (Fix fs b) = Fix (fmap (second untagCore) fs) (untagCore b)
        untagCoreN (Tuple ts) = Tuple ts
        untagCoreN (Lam n a) = Lam n (untagCore a)
        untagCoreN (Match t n cs) = Match t n (fmap (\(a,b,c)->(a,NoTag,untagCore c)) cs)
        untagCoreN (Annot a t) = Annot (untagCore a) t
        untagCoreN (Val v) = Val v
        untagCoreN (Cons i vs) = Cons i vs
        untagCoreN (Prim p vs) = Prim p vs
        untagCoreN (Error s) = Error s

{-
aconv :: Map.Map Name Name -> Core t -> Core t
aconv m (Val (Var (LocalIdentifier n))) = Val . Var . LocalIdentifier $ Map.findWithDefault n n m
aconv m (Let n a b) = Let n (aconv m a) (aconv m b)
aconv m (Fix f e) = Fix (map (second (aconv m)) f) (aconv m e)
aconv m (Lam n e) = Lam n (aconv m e)
aconv m (Match n p) = Match n (map (\(a,b,c) -> (a,b,aconv m c)) p)
aconv m (Annot a t) = Annot (aconv m a) t
aconv _ x = x
-}

instance Show (CoreNode tag) where
    show (Let n ds ir) = "let " ++ show n ++ " = " ++ show ds ++ " in\n" ++ show ir
    show (Fix ds ir) = "fix " ++ intercalate "\n    " (fmap (\(n, ir) -> show n ++ " = " ++ show ir) ds) ++ " in\n" ++ show ir
    show (Cons id args) = show id ++ "{" ++ concatMap ((',':) . show) args ++ "}"
    show (Tuple exprs) = "{" ++ intercalate "," (fmap show exprs) ++ "}"
    show (Select i v) = '#':show i ++ "(" ++ show v ++ ")"
    show (Annot ir ty) = "(" ++ show ir ++ " :: " ++ show ty ++ ")"
    show (Lam n ir) = "(\\" ++ show n ++ ". " ++ show ir ++ ")"
    show (Val v) = show v
    show (Match (Nothing,_) ir cases) = "match " ++ show ir ++ " with\n" ++ concatMap (\(p, _, ir) -> "    " ++ show p ++ " -> " ++ show ir ++ "\n") cases
    show (Match (Just t,_) ir cases) = "match " ++ show ir ++ " :: " ++ show t ++ " with\n" ++ concatMap (\(p, _, ir) -> "    " ++ show p ++ " -> " ++ show ir ++ "\n") cases
    show (Error s) = "error " ++ show s

instance Show tag => Pretty (CoreNode tag) Int where
    showtag (Let _ _ _) _ = True
    showtag (Fix _ _) _ = True
    showtag (Lam _ _) _ = True
    showtag _ _ = False

    pretty (Let v ds ir) n = "\n" ++ replicate n ' ' ++ "let " ++ show v ++ " = " ++ pretty ds (n+4) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix ds ir) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> show v ++ " = " ++ pretty ir (n+8)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Annot ir ty) n = "(" ++ pretty ir n ++ " :: " ++ show ty ++ ")"
    pretty (Lam v ir) n = "(\\" ++ show v ++ ". " ++ pretty ir n ++ ")"
    pretty (Val i) _ = show i
    pretty (Cons id args) _ = show id ++ "{" ++ concatMap ((',':) . show) args ++ "}"
    pretty (Select i v) n = '#':show i ++ "(" ++ show v ++ ")"
    pretty (Tuple xs) n = "{" ++ intercalate "," (fmap show xs) ++ "}"
    pretty (Match (Nothing,_) ir cases) n = "match " ++ show ir ++ " with\n" ++ intercalate "\n" (fmap (\(p, _, ir) -> replicate n ' ' ++ show p ++ " -> " ++ pretty ir (n+4)) cases)
    pretty (Match (Just t,_) ir cases) n = "match " ++ show ir ++ " :: " ++ show t ++ " with\n" ++ intercalate "\n" (fmap (\(p, _, ir) -> replicate n ' ' ++ show p ++ " -> " ++ pretty ir (n+4)) cases)
    pretty (Error s) _ = "error " ++ show s

instance (Substitutable tag) => Substitutable (CoreNode tag) where
    apply s (Fix ds ir) = Fix (fmap (\(a, b) -> (a, apply s b)) ds) (apply s ir)
    apply s (Let n ds ir) = Let n (apply s ds) (apply s ir)
    apply s (Annot ir ty) = Annot (apply s ir) (apply s ty)
    apply s (Lam n ir) = Lam n (apply s ir)
    apply s (Match (ty,p) v cases) = Match (fmap (apply s) ty,p) v (fmap (\(a, t, b) -> (a, apply s t, apply s b)) cases)
    apply _ x = x

    ftv (Fix ds ir) = mconcat (fmap (\(_, b) -> ftv b) ds) `mappend` ftv ir
    ftv (Let n ds ir) = ftv ds `mappend` ftv ir
    ftv (Annot ir ty) = ftv ir `mappend` ftv ty
    ftv (Lam _ ir) = ftv ir
    ftv (Match (t,_) _ cases) = mconcat (fmap (\(_, _, b) -> ftv b) cases) `mappend` foldr mappend mempty (fmap ftv t)
    ftv _ = mempty