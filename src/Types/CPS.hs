{-# LANGUAGE MultiParamTypeClasses #-}
module Types.CPS where

import Types.Ident
import Types.Prim
import Types.Pretty
import Types.Type

import Data.List (intercalate)
import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

data CFunData
    = CFunData
    { iscont :: Bool
    , islet :: Bool
    , issplit :: Bool
    , isexport :: Bool
    , name :: Maybe Identifier
    } deriving(Eq)

cfun :: CFunData
cfun = CFunData False False False False Nothing

data CFun
    = Fun CFunData Identifier [Identifier] CExp
    deriving(Eq)

instance Show CFun where
    show (Fun _ n args exp) = show n ++ concatMap ((' ':) . show) args ++ " = " ++ show exp

data AccessPath
    = NoPath
    | SelPath Int AccessPath
    deriving(Eq)

{-
CPS 'Boxity' Options:
- all forms of 'boxing' (i.e. tagging for chars, ints, etc., boxing for vectors, refs, etc.) represented similarly in CPS

Option 1:
- Explicit boxing/unboxing, by introducing to CExp:
    + Enbox Value LitType Identifier CExp
    + Unbox Value LitType Identifier CExp
- more register usage, so more spills
- unboxing is a 'cached' operation

Option 2:
- Values are boxed/unboxed as needed, by introducing as CValue:
    + Enbox Value LitType
    + Unbox Value LitType
    + Value Value CType
- less register useage, fewer spills
- unboxing is not cached
- somtimes improbable to be able to unbox inline (i.e. with vectors)

Option 3:
- Both! - opportunity for best-of-both-worlds

Option 4:
- None! - will take this approach currently

- need to look into expensiveness of boxing/unboxing - see ocaml?

Expressing variadics with CType:
- `Switch` also quantifies over variadic being `Switch`d and the type of that variadic.
- `CExp` alternatives are zipped with their respective `[CType]` alternative.
-}

data CType
    = Unboxed LitType
    | Boxed LitType
    | TuplePtr [CType]
    | VariantPtr [[CType]]
    | Address
    | Undefined
    deriving(Eq)

selectTy :: Int -> CType -> CType
selectTy 0 (VariantPtr _) = Boxed IntTy
selectTy 0 (TuplePtr (x:_)) = x
selectTy n (TuplePtr (_:xs)) = selectTy (n-1) (TuplePtr xs)
selectTy i x = error $ "cannot select type at index " ++ show i ++ " from " ++ show x

instance Show CType where
    show (Unboxed t) = '#':show t
    show (Boxed t) = show t
    show (TuplePtr xs) = "{" ++ concatMap ((++",") . show) xs ++ "}"
    show (VariantPtr xss) = "(" ++ intercalate " | " (fmap (("{"++) . (++"}") . concatMap ((++",") . show)) xss)
    show Undefined = "Undefined"
    show Address = "Addr"

data CValue
    = CValue Value CType
    deriving(Eq)

instance Show CValue where
    show (CValue v t) = show v ++ " :: " ++ show t

data CExp
    = App Value [Value]
    | Fix [CFun] CExp
    | Record [(Value, AccessPath)] Identifier CExp
    | Select Int Value Identifier CExp
    | Switch Value [CExp]
    | Error String
    | Halt
    | Primop Primop [Value] Identifier [CExp]
    deriving(Eq)

getEscaping :: CExp -> Set.Set Identifier
getEscaping (App _ vs) = Set.fromList $ extractLabels vs
getEscaping (Fix fs e) = mconcat (getEscaping e:fmap (\(Fun _ _ _ e) -> getEscaping e) fs)
getEscaping (Record vs _ e) = Set.fromList (extractLabels $ fmap fst vs) `mappend` getEscaping e
getEscaping (Select _ v _ e) = Set.fromList (extractLabels [v]) `mappend` getEscaping e
getEscaping (Switch v es) = mconcat (Set.fromList (extractLabels [v]):fmap getEscaping es)
getEscaping (Primop _ vs _ es) = mconcat (Set.fromList (extractLabels vs):fmap getEscaping es)
getEscaping _ = mempty

extractIdents :: [Value] -> [Identifier]
extractIdents (Var id:ns) = id:extractIdents ns
extractIdents (_:xs) = extractIdents xs
extractIdents [] = []

extractLabels :: [Value] -> [Identifier]
extractLabels (Label id:ns) = id:extractLabels ns
extractLabels (_:xs) = extractLabels xs
extractLabels [] = []

fv :: CExp -> Set.Set Identifier
fv (App i vs) = Set.fromList (extractIdents $ i:vs)
fv (Fix fns exp) = flip Set.difference (Set.fromList . fmap (\(Fun _ id _ _) -> id) $ fns) $ Set.union (fv exp) . mconcat $ fmap (\(Fun _ _ args exp) -> fv exp `Set.difference` Set.fromList args) fns
fv (Record vs n exp) = Set.delete n $ fv exp `Set.union` (Set.fromList . extractIdents $ fmap fst vs)
fv (Select _ v n exp) = Set.delete n $ Set.fromList (extractIdents [v]) `Set.union` fv exp
fv (Switch v exps) = mconcat (fmap fv exps) `Set.union` Set.fromList (extractIdents [v])
fv (Primop _ args n exps) = Set.delete n $ mconcat (fmap fv exps) `Set.union` Set.fromList (extractIdents args)
fv _ = mempty

isLet :: CExp -> Bool
isLet (Select _ _ _ _) = True
isLet (Record _ _ _) = True
isLet (Primop _ _ _ _) = True
isLet _ = False

instance Show CExp where
    show (App a args) = show a ++ concatMap (\arg -> ' ':show arg) args
    show (Fix defs exp) = "fix " ++ concatMap (\fn -> show fn ++ "\n") defs ++ "in " ++ show exp
    show (Record va id exp) = "let " ++ show id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "} in " ++ show exp
    show (Select i v id exp) = "let " ++ show id ++ " = " ++ show v ++ "[" ++ show i ++ "]" ++ " in " ++ show exp
    show (Switch v exp) = "switch " ++ show v ++ concatMap (\cse -> "\n" ++ show cse) exp
    show (Primop p args n xs) = "let " ++ show n ++ " = " ++ show p ++ "{" ++ intercalate "," (fmap show args) ++ "} in" ++ concatMap (\cse -> "\n" ++ show cse) xs
    show Halt = "halt"
    show (Error s) = "error '" ++ s ++ "'"

instance Show AccessPath where
    show NoPath = ""
    show (SelPath x y) = show y ++ "[" ++ show x ++ "]"

instance Pretty CFun Int where
    showtag _ _ = False

    pretty (Fun _ v args exp) n = intercalate " " (show v:fmap show args) ++ " = " ++ pretty exp (n+4)

instance Pretty CExp Int where
    showtag _ _ = False

    pretty (App a args) _ = show a ++ concatMap (\arg -> ' ':show arg) args
    --"\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\(v, ir) -> v ++ " = " ++ pretty ir (n+4)) ds) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty ir (n+4)
    pretty (Fix defs exp) n = "\n" ++ replicate n ' ' ++ "fix " ++ intercalate ("\n" ++ replicate (n+4) ' ') (fmap (\fn -> pretty fn (n+4)) defs) ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Record va id exp) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "}" ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = {" ++ intercalate "," (fmap (\(v,a) -> show v ++ show a) va) ++ "}\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Select i v id exp) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = " ++ "#" ++ show i ++ "(" ++ show v ++ ")" ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = " ++ "#" ++ show i ++ "(" ++ show v ++ ")\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Switch v exp) n = "\n" ++ replicate n ' ' ++ "switch " ++ show v ++ concatMap (\(cse, i) -> "\n" ++ replicate (n+4) ' ' ++ show i ++ " -> " ++ pretty cse (n+8)) (zip exp [0..])
    pretty Halt _ = "halt"
    pretty (Error s) _ = "error '" ++ s ++ "'"
    pretty (Primop op vs id [exp]) n
        | isLet exp = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = " ++ show op ++ concatMap ((' ':) . show) vs ++ pretty exp n
        | otherwise = "\n" ++ replicate n ' ' ++ "let " ++ show id ++ " = " ++ show op ++ concatMap ((' ':) . show) vs ++ "\n" ++ replicate n ' ' ++ "in " ++ pretty exp (n+4)
    pretty (Primop op vs _ exps) n = "\n" ++ replicate n ' ' ++ show op ++ "{" ++ concatMap ((++",") . show) vs ++ "}{" ++ concatMap ((++",") . (`pretty` (n+4))) exps ++ "}"

-- get whether the variable is called
calledIn :: CExp -> Identifier -> Bool
calledIn (App (Var m) _) n = n == m
calledIn (Fix _ e) n = calledIn e n
calledIn (Record _ _ e) n = calledIn e n
calledIn (Select _ _ _ e) n = calledIn e n
calledIn (Switch _ es) n = or $ fmap (`calledIn` n) es
calledIn (Primop _ _ _ es) n = or $ fmap (`calledIn` n) es
calledIn _ _ = False