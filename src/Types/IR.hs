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

data Ind
    = Ind Identifier (Maybe Kind) [(Identifier, Scheme)]

instance Show Ind where
    show (Ind n mk ts) =
        let header = "ind " ++ show n
            annot = case mk of
                Just x -> " :: " ++ show x ++ "\n"
                Nothing -> "\n"
            body = concatMap (\(id, typ) -> "    " ++ show id ++ " :: " ++ show typ ++ "\n") ts in
                header ++ annot ++ body
    showList = const . concatMap ((++"\n") . show)

data IRPattern
    = IRCons Identifier [Name]
    | IRWild
    deriving(Eq)

instance Show IRPattern where
    show (IRCons i ns) = show i ++ concatMap (\n -> " " ++ n) ns
    show IRWild = "_"

type PolyIR typ tag = TaggedAppGraph tag (PolyIRNode typ tag)
data PolyIRNode typ tag
    = Let Name (PolyIR typ tag) (PolyIR typ tag) -- also allow binding externals here??
    | Fix [(Identifier, PolyIR typ tag)] (PolyIR typ tag) -- need to allow for binding external identifiers here?
    | Cons Identifier [Name]
    | Annot (PolyIR typ tag) typ
    | Lam Name (PolyIR typ tag)
    | Var Identifier
    | Unboxed UnboxedLit
    | Match Name [(IRPattern, PolyIR typ tag)]
    | Select Int (PolyIR typ tag)
    | Primop Primop
    deriving(Eq)

instance (Show typ, Show tag) => Show (PolyIRNode typ tag) where
    show (Let n ds ir) = "let " ++ n ++ " = " ++ show ds ++ " in\n" ++ show ir
    show (Fix ds ir) = "fix " ++ intercalate "\n    " (fmap (\(n, ir) -> show n ++ " = " ++ show ir) ds) ++ " in\n" ++ show ir
    show (Cons id args) = "{" ++ show id ++ concatMap (',':) args ++ "}"
    show (Annot ir ty) = "(" ++ show ir ++ " :: " ++ show ty ++ ")"
    show (Lam n ir) = "(\\" ++ n ++ ". " ++ show ir ++ ")"
    show (Var id) = show id
    show (Unboxed l) = show l
    show (Match ir cases) = "match " ++ ir ++ " with\n" ++ concatMap (\(p, ir) -> "    " ++ show p ++ " -> " ++ show ir ++ "\n") cases
    show (Select i exp) = "#" ++ show i ++ "(" ++ show exp ++ ")"

instance (Show typ, Show tag) => Pretty (PolyIRNode typ tag) Int where
    pretty (Let v ds ir) n = "\n" ++ replicate n ' ' ++ "let " ++ v ++ " = " ++ pretty ds (n+4) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix ds ir) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> show v ++ " = " ++ pretty ir (n+8)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Annot ir ty) n = "(" ++ pretty ir n ++ " :: " ++ show ty ++ ")"
    pretty (Lam v ir) n = "(\\" ++ v ++ ". " ++ pretty ir n ++ ")"
    pretty (Var id) _ = show id
    pretty (Unboxed l) _ = show l
    pretty (Cons id args) _ = "{" ++ show id ++ concatMap (',':) args ++ "}"
    pretty (Match ir cases) n = "match " ++ ir ++ " with\n" ++ intercalate "\n" (fmap (\(p, ir) -> replicate n ' ' ++ show p ++ " -> " ++ pretty ir (n+4)) cases)
    pretty (Select i exp) n = "#" ++ show i ++ "(" ++ pretty exp (n+4) ++ ")"

instance (Substitutable typ, Substitutable tag) => Substitutable (PolyIRNode typ tag) where
    apply s (Fix ds ir) = Fix (fmap (\(a, b) -> (a, apply s b)) ds) (apply s ir)
    apply s (Let n ds ir) = Let n (apply s ds) (apply s ir)
    apply s (Annot ir ty) = Annot (apply s ir) (apply s ty)
    apply s (Lam n ir) = Lam n (apply s ir)
    apply s (Match v cases) = Match v (fmap (\(a, b) -> (a, apply s b)) cases)
    apply _ x = x

    ftv (Fix ds ir) = mconcat (fmap (\(_, b) -> ftv b) ds) `mappend` ftv ir
    ftv (Let n ds ir) = ftv ds `mappend` ftv ir
    ftv (Annot ir ty) = ftv ir `mappend` ftv ty
    ftv (Lam _ ir) = ftv ir
    ftv (Match v cases) = mconcat (fmap (\(_, b) -> ftv b) cases) `mappend` (Set.singleton v)
    ftv _ = mempty

type IRNode = PolyIRNode Scheme NoTag
type IR = AppGraph IRNode