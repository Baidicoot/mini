{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Types.CPS where

import Types.Ident
import Types.Type
import Types.Prim
import Types.Pretty

import Data.List (intercalate)
import Data.Maybe (maybeToList, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map

data Value
    = Var Identifier
    | Unboxed UnboxedLit
    deriving(Eq)

instance Show Value where
    show (Var id) = show id
    show (Unboxed u) = show u

data CFun
    = Fun Identifier [Name] CExp
    deriving(Eq)

instance Show CFun where
    show (Fun n args exp) = show n ++ concatMap (\arg -> ' ':arg) args ++ " = " ++ show exp

data AccessPath
    = OffPath Int
    | SelPath Int AccessPath
    deriving(Eq)

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Name CExp
    | Select Int Value Name CExp
    -- | Offset Int Value Name CExp
    | Switch Value [CExp]
    | MatchError
    | Halt
    | Primop Primop [Value] Name [CExp]
    deriving(Eq)

valueToName :: Value -> Maybe Name
valueToName = (\case
    Var (LocalIdentifier id) -> Just id
    _ -> Nothing)

identToName :: Identifier -> Maybe Name
identToName = (\case
    LocalIdentifier id -> Just id
    _ -> Nothing)

fv :: CExp -> Set.Set Name
fv (App n vs) = Set.fromList . catMaybes . fmap valueToName $ (n:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . catMaybes . fmap (\(Fun id _ _) -> identToName id) $ fns) $ Set.union (fv exp) . mconcat $ fmap (\(Fun _ args exp) -> fv exp `Set.difference` Set.fromList args) fns
fv (Record vs n exp) = Set.delete n $ (fv exp) `Set.union` (Set.fromList . catMaybes . fmap (valueToName . fst) $ vs)
fv (Select _ v n exp) = Set.delete n $ Set.fromList (maybeToList (valueToName v)) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` (Set.fromList . maybeToList . valueToName $ v)
fv (Primop _ args n exps) = Set.delete n $ mconcat (fmap fv exps) `Set.union` (Set.fromList . catMaybes . fmap valueToName $ args)
fv _ = mempty

mzip :: Monoid m => [m] -> [m] -> [m]
mzip (a:as) (b:bs) = (a `mappend` b):(mzip as bs)
mzip as [] = as
mzip [] bs = bs

type VarDepth = Map.Map Name Int

valuesToDepth = Map.fromList . fmap (\x -> (x, 0)) . catMaybes . fmap valueToName

getVarDepth :: CExp -> VarDepth
getVarDepth (App n vs) = valuesToDepth (n:vs)
getVarDepth (Record vs n exp) = fmap (+1) . Map.delete n $ getVarDepth exp
getVarDepth (Select _ v n exp) = Map.union (valuesToDepth [v]) . fmap (+1) $ getVarDepth exp
getVarDepth (Switch v exps) = Map.union (valuesToDepth [v]) . fmap (+1) . mconcat $ fmap getVarDepth exps
getVarDepth (Primop _ args n exps) = Map.union (valuesToDepth args) . fmap (+1) . Map.delete n . mconcat $ fmap getVarDepth exps
getVarDepth _ = Map.empty

type VarInfo = (Set.Set Name, VarDepth)

getVarInfo :: CExp -> VarInfo
getVarInfo exp = (fv exp, getVarDepth exp)

{-
orderByDepth :: CExp -> VarDepth
orderByDepth (App n vs) = Map.fromList . fmap (\x -> (x, 0)) . catMaybes . fmap valueToName $ (n:vs)
orderByDepth (Record vs n exp) = fmap (fmap (+1) . Map.delete n) $ (Map.fromList . fmap (\x -> (x, 0)) . catMaybes . fmap (valueToName . fst) $ vs):(orderByDepth exp)
orderByDepth (Select _ v n exp) = fmap (fmap (+1) . Map.delete n) $ (Map.singleton v 0):(orderByDepth exp)
orderByDepth (Switch v exps) = (Map.fromList . fmap (\x -> (x, 0)) . maybeToList . valueToName $ v):(foldr mzip [] (fmap orderByDepth exps))
orderByDepth (Primop _ args n exps) = fmap (Map.delete n) $ (Map.fromList . fmap (\x -> (x, 0)) . catMaybes . fmap valueToName $ args):(foldr mzip [] (fmap orderByDepth exps))
orderByDepth _ = Map.empty
-}

instance Show CExp where
    show (App a args) = show a ++ concatMap (\arg -> ' ':show arg) args
    show (Fix defs exp) = "fix " ++ concatMap (\fn -> show fn ++ "\n") defs ++ "in " ++ show exp
    show (Record va id exp) = "let " ++ id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show a ++ "=" ++ show v) va) ++ "} in " ++ show exp
    show (Select i v id exp) = "let " ++ id ++ " = " ++ show v ++ "[" ++ show i ++ "]" ++ " in " ++ show exp
    show (Switch v exp) = "switch " ++ show v ++ concatMap (\cse -> "\n" ++ show cse) exp
    show Halt = "halt"
    show MatchError = "fail"

instance Show AccessPath where
    show (OffPath x) = show x
    show (SelPath x y) = show y ++ "[" ++ show x ++ "]"

instance Pretty CFun Int where
    pretty (Fun v args exp) n = intercalate " " (show v:args) ++ " = " ++ pretty exp (n+4)

instance Pretty CExp Int where
    pretty (App a args) _ = show a ++ concatMap (\arg -> ' ':show arg) args
    --"\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> v ++ " = " ++ pretty ir (n+4)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix defs exp) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\fn -> pretty fn (n+4)) defs) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Record va id exp) n = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show a ++ "=" ++ show v) va) ++ "}\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Select i v id exp) n = "\n" ++ replicate n ' ' ++ "let " ++ id ++ " = " ++ "#" ++ show i ++ "(" ++ show v ++ ")" ++ " in " ++ pretty exp (n+4)
    pretty (Switch v exp) n = "\n" ++ replicate n ' ' ++ "switch " ++ show v ++ concatMap (\(cse, i) -> "\n" ++ replicate (n+4) ' ' ++ show i ++ " -> " ++ pretty cse (n+8)) (zip exp [0..])
    pretty Halt _ = "halt"
    pretty MatchError _ = "fail"