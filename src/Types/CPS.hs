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

valueToName :: Value -> Maybe Name
valueToName (Var (LocalIdentifier id)) = Just id
valueToName _ = Nothing

extractNames :: [Value] -> [Name]
extractNames (Var (LocalIdentifier n):ns) = n:extractNames ns
extractNames (_:xs) = extractNames xs
extractNames [] = []

extractIdents :: [Value] -> [Identifier]
extractIdents (Var id:ns) = id:extractIdents ns
extractIdents (_:xs) = extractIdents xs
extractIdents [] = []

fv :: CExp -> Set.Set Name
fv (App n vs) = Set.fromList . mapMaybe valueToName $ (n:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . extractLocals . fmap (\(Fun id _ _) -> id) $ fns) $ Set.union (fv exp) . mconcat $ fmap (\(Fun _ args exp) -> fv exp `Set.difference` Set.fromList args) fns
fv (Record vs n exp) = Set.delete n $ fv exp `Set.union` (Set.fromList . mapMaybe (valueToName . fst) $ vs)
fv (Select _ v n exp) = Set.delete n $ Set.fromList (maybeToList (valueToName v)) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` (Set.fromList . maybeToList . valueToName $ v)
fv (Primop _ args n exps) = Set.delete n $ mconcat (fmap fv exps) `Set.union` (Set.fromList . mapMaybe valueToName $ args)
fv _ = mempty

type VarDepth = Map.Map Name Int

valuesToDepth :: [Value] -> Map.Map Name Int
valuesToDepth = Map.fromList . fmap (\x -> (x, 0)) . mapMaybe valueToName

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

-- get the argument positions a variable is used in
getArg :: Name -> CExp -> Set.Set Int
getArg n (App _ vs) = Set.fromList . fmap fst . filter (\(i,v) -> v == Var (LocalIdentifier n)) $ zip [0..] vs
getArg n (Record _ _ e) = getArg n e
getArg n (Select _ _ _ e) = getArg n e
getArg n (Switch _ es) = mconcat $ fmap (getArg n) es
getArg n (Primop _ vs _ es) = (Set.fromList . fmap fst . filter (\(i,v) -> v == Var (LocalIdentifier n)) $ zip [0..] vs) `Set.union` (mconcat $ fmap (getArg n) es)
getArg _ _ = mempty

-- get whether the variable is called
getCalled :: Name -> CExp -> Bool
getCalled n (App (Var (LocalIdentifier m)) _) = n == m
getCalled n (Record _ _ e) = getCalled n e
getCalled n (Select _ _ _ e) = getCalled n e
getCalled n (Switch _ es) = or $ fmap (getCalled n) es
getCalled n (Primop _ _ _ es) = or $ fmap (getCalled n) es
getCalled _ _ = False

maxArgs :: CExp -> Int
maxArgs (App _ vs) = length vs
maxArgs (Record _ _ e) = maxArgs e
maxArgs (Select _ _ _ e) = maxArgs e
maxArgs (Switch _ es) = foldr max 0 $ fmap maxArgs es
maxArgs (Primop _ _ _ es) = foldr max 0 $ fmap maxArgs es
maxArgs _ = 0

getRegs :: Name -> CExp -> Set.Set Int
getRegs n e = (if getCalled n e then Set.insert 0 else id) (Set.map (+1) (getArg n e))