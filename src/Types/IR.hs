module Types.IR where

import Types.Ident
import Types.Graph
import Types.Pattern
import Types.Type
import Data.List (intercalate)

data UnboxedLit
    = Int Int
    deriving(Eq)

instance Show UnboxedLit where
    show (Int i) = '#':(show i)

type PolyIR typ tag = TaggedAppGraph tag (PolyIRNode typ tag)
data PolyIRNode typ tag
    = Let [(Name, PolyIR typ tag)] (PolyIR typ tag)
    | Annot (PolyIR typ tag) typ
    | Lam Name (PolyIR typ tag)
    | Var Identifier
    | Unboxed UnboxedLit
    | Match (PolyIR typ tag) [(Pattern, PolyIR typ tag)]
    deriving(Eq)

instance (Show typ, Show tag) => Show (PolyIRNode typ tag) where
    show (Let ds ir) = "let " ++ intercalate "\n    " (fmap (\(n, ir) -> n ++ " = " ++ show ir) ds) ++ " in\n" ++ show ir
    show (Annot ir ty) = "(" ++ show ir ++ " :: " ++ show ty ++ ")"
    show (Lam n ir) = "(\\" ++ n ++ ". " ++ show ir ++ ")"
    show (Var id) = show id
    show (Unboxed l) = show l
    show (Match ir cases) = "match " ++ show ir ++ " with\n" ++ concatMap (\(p, ir) -> "    " ++ show p ++ " -> " ++ show ir ++ "\n") cases

instance (Substitutable typ, Substitutable tag) => Substitutable (PolyIRNode typ tag) where
    apply s (Let ds ir) = Let (fmap (\(a, b) -> (a, apply s b)) ds) (apply s ir)
    apply s (Annot ir ty) = Annot (apply s ir) (apply s ty)
    apply s (Lam n ir) = Lam n (apply s ir)
    apply s (Match ir cases) = Match (apply s ir) (fmap (\(a, b) -> (a, apply s b)) cases)
    apply _ x = x

    ftv (Let ds ir) = mconcat (fmap (\(_, b) -> ftv b) ds) `mappend` ftv ir
    ftv (Annot ir ty) = ftv ir `mappend` ftv ty
    ftv (Lam _ ir) = ftv ir
    ftv (Match ir cases) = mconcat (fmap (\(_, b) -> ftv b) cases) `mappend` ftv ir
    ftv _ = mempty

type IRNode = PolyIRNode Scheme ()
type IR = AppGraph IRNode