module Checker.TypeTree (Type, DSType, addapp, mktree) where
-- responsible for constructing type trees
import qualified Parser.Syntax as S

import Prelude hiding(Left, Right)

data DSType
    = DOp DSOp
    | DPar [DSType]
    | DEmpty
    | DNamed String
    | DVar String
    deriving(Eq, Show)

data DSOp
    = DApp
    | DProd
    | DFunc
    deriving(Eq, Show)

conv :: S.Type -> DSType
conv S.TSeq = DOp DProd
conv S.TFun = DOp DFunc
conv (S.TNam s) = DNamed s
conv (S.TVar s) = DVar s
conv (S.TPar xs) = DPar (addapp False xs)

isntOp :: S.Type -> Bool
isntOp S.TSeq = False
isntOp S.TFun = False
isntOp _ = True

addapp :: Bool -> [S.Type] -> [DSType]
addapp hadntOp (x:xs)
    | (not hadntOp) && (not (isntOp x)) = DEmpty:(conv x):(addapp (isntOp x) xs)
    | isntOp x && hadntOp = (DOp DApp):(conv x):(addapp (isntOp x) xs)
    | otherwise = (conv x):(addapp (isntOp x) xs)
addapp False [] = [DEmpty]
addapp True _ = []

data Type
    = App Type Type
    | Seq [Type]
    | Func
    | Named String
    | Var String
    deriving(Eq)

pshow :: Type -> String
pshow (Named s) = s
pshow (Var s) = s
pshow Func = "Fun"
pshow (Seq xs) = show xs
pshow x = "(" ++ show x ++ ")"

instance Show Type where
    show (App (App Func a) b) = pshow a ++ " -> " ++ show b
    show (App a b) = show a ++ " " ++ pshow b
    show x = pshow x

-- Precedence climbing method - https://en.wikipedia.org/wiki/Operator-precedence_parser
data Assoc
    = Right
    | Left
    deriving(Eq, Show)

precedence :: DSOp -> Int
precedence DApp = 3
precedence DProd = 2
precedence DFunc = 1

assoc :: DSOp -> Assoc
assoc DApp = Left
assoc DProd = Left
assoc DFunc = Right

mktype :: DSType -> Type
mktype (DVar s) = Var s
mktype (DNamed s) = Named s
mktype (DPar p) = parse p
mktype DEmpty = Seq []
mktype (DOp _) = error "can not convert op"

apply :: DSOp -> Type -> Type -> Type
apply DApp a b = App a b
apply DFunc a b = App (App Func a) b
apply DProd (Seq xs) b = Seq (xs ++ [b])
apply DProd a b = Seq [a, b]

type Parser s t = s -> (t, s)

next :: Parser [a] (Maybe a)
next (x:xs) = (Just x, xs)
next [] = (Nothing, [])

parse :: [DSType] -> Type
parse (x:xs) = let (res, []) = internal (mktype x) 0 xs in res
    where
        internal :: Type -> Int -> Parser [DSType] Type
        internal lhs prec xs = case next xs of
            (Just (DOp o), xs') | precedence o >= prec ->
                let (Just rhs, xs'') = next xs'
                    (rhs', xs''') = inside (mktype rhs) (precedence o) xs'' in
                        internal (apply o lhs rhs') prec xs'''
            _ -> (lhs, xs)
        inside :: Type -> Int -> Parser [DSType] Type
        inside rhs prec xs = case next xs of
            (Just (DOp o), _) | if assoc o == Right then precedence o == prec else precedence o > prec ->
                let (rhs', xs') = internal rhs (precedence o) xs in inside rhs' prec xs'
            _ -> (rhs, xs)
        
mktree :: [S.Type] -> Type
mktree xs = parse (addapp False xs)