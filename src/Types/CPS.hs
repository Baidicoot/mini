{-# LANGUAGE MultiParamTypeClasses #-}
module Types.CPS where

import Types.Ident
import Types.Prim
import Types.Pretty

import Data.List (intercalate)
import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

data CFun
    = Fun Identifier [Name] CExp
    deriving(Eq)

instance Show CFun where
    show (Fun n args exp) = show n ++ concatMap (' ':) args ++ " = " ++ show exp

data AccessPath
    = NoPath
    | SelPath Int AccessPath
    deriving(Eq)

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Name CExp
    | Select Int Value Name CExp
    | Switch Value [CExp]
    | Error String
    | Halt
    | Primop Primop [Value] Name [CExp]
    deriving(Eq)

{-
extractNames :: [Value] -> [Name]
extractNames (Var (LocalIdentifier n):ns) = n:extractNames ns
extractNames (_:xs) = extractNames xs
extractNames [] = []
-}
{-
extractExterns :: [Value] -> [Identifier]
extractExterns (Var (LocalIdentifier _):ns) = extractExterns ns
extractExterns (Var i:ns) = i:extractExterns ns
extractExterns [] = []
-}
extractIdents :: [Value] -> [Identifier]
extractIdents (Var id:ns) = id:extractIdents ns
extractIdents (_:xs) = extractIdents xs
extractIdents [] = []

extractLabels :: [Value] -> [Identifier]
extractLabels (Label id:ns) = id:extractIdents ns
extractLabels (_:xs) = extractIdents xs
extractLabels [] = []

fv :: CExp -> Set.Set Identifier
fv (App i vs) = Set.fromList (extractIdents $ i:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . fmap (\(Fun id _ _) -> id) $ fns) $ Set.union (fv exp) . mconcat $ fmap (\(Fun _ args exp) -> fv exp `Set.difference` Set.fromList (fmap LocalIdentifier args)) fns
fv (Record vs n exp) = Set.delete (LocalIdentifier n) $ fv exp `Set.union` (Set.fromList . extractIdents $ fmap fst vs)
fv (Select _ v n exp) = Set.delete (LocalIdentifier n) $ Set.fromList (extractIdents [v]) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` Set.fromList (extractIdents [v])
fv (Primop _ args n exps) = Set.delete (LocalIdentifier n) $ mconcat (fmap fv exps) `Set.union` Set.fromList (extractIdents args)
fv _ = mempty

isLet :: CExp -> Bool
isLet (Select _ _ _ _) = True
isLet (Record _ _ _) = True
isLet (Primop _ _ _ _) = True
isLet _ = False

instance Show CExp where
    show (App a args) = show a ++ concatMap (\arg -> ' ':show arg) args
    show (Fix defs exp) = "fix " ++ concatMap (\fn -> show fn ++ "\n") defs ++ "in " ++ show exp
    show (Record va id exp) = "let " ++ id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "} in " ++ show exp
    show (Select i v id exp) = "let " ++ id ++ " = " ++ show v ++ "[" ++ show i ++ "]" ++ " in " ++ show exp
    show (Switch v exp) = "switch " ++ show v ++ concatMap (\cse -> "\n" ++ show cse) exp
    show Halt = "halt"
    show (Error s) = "error '" ++ s ++ "'"

instance Show AccessPath where
    show NoPath = ""
    show (SelPath x y) = show y ++ "[" ++ show x ++ "]"

instance Pretty CFun Int where
    showtag _ _ = False

    pretty (Fun v args exp) n = intercalate " " (show v:args) ++ " = " ++ pretty exp (n+4)

instance Pretty CExp Int where
    showtag _ _ = False

    pretty (App a args) _ = show a ++ concatMap (\arg -> ' ':show arg) args
    --"\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> v ++ " = " ++ pretty ir (n+4)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix defs exp) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\fn -> pretty fn (n+4)) defs) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Record va id exp) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "}" ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "}\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Select i v id exp) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ "#" ++ show i ++ "(" ++ show v ++ ")" ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ "#" ++ show i ++ "(" ++ show v ++ ")\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Switch v exp) n = "\n" ++ replicate n ' ' ++ "switch " ++ show v ++ concatMap (\(cse, i) -> "\n" ++ replicate (n+4) ' ' ++ show i ++ " -> " ++ pretty cse (n+8)) (zip exp [0..])
    pretty Halt _ = "halt"
    pretty (Error s) _ = "error '" ++ s ++ "'"
    pretty (Primop op vs id [exp]) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ show op ++ concatMap ((' ':) . show) vs ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ show op ++ concatMap ((' ':) . show) vs ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)

-- get whether the variable is called
calledIn :: CExp -> Identifier -> Bool
calledIn (App (Var m) _) n = n == m
calledIn (Record _ _ e) n = calledIn e n
calledIn (Select _ _ _ e) n = calledIn e n
calledIn (Switch _ es) n = or $ fmap (`calledIn` n) es
calledIn (Primop _ _ _ es) n = or $ fmap (`calledIn` n) es
calledIn _ _ = False