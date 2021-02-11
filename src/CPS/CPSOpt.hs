{- assorted optimisations to occur after closure conversion, but before spilling -}
module CPS.CPSOpt (cpsOpt) where

import Types.CPS
import Types.Ident
import Types.Prim
import Control.Arrow
import Data.List
import Types.Build

cpsOpt :: OptFlags -> CExp -> CExp
cpsOpt OptFlags{cps_inline_singles = True} = simpleInline
cpsOpt _ = id

-- inlining
usesOf :: Identifier -> CExp -> Int
usesOf i (App (Label j) _) | i == j = 1
usesOf i (Fix fns e) =
    sum (fmap (\(Fun _ _ _ e) -> usesOf i e) fns) + usesOf i e
usesOf i (Record _ _ e) = usesOf i e
usesOf i (Select _ _ _ e) = usesOf i e
usesOf i (Switch _ es) = sum (fmap (usesOf i) es)
usesOf i (Primop _ _ _ es) = sum (fmap (usesOf i) es)
usesOf _ _ = 0

substVarVal :: Identifier -> Value -> Value -> Value
substVarVal i v (Var j) | i == j = v
substVarVal _ _ x = x

substVar :: Identifier -> Value -> CExp -> CExp
substVar i v (App f vs) = App (substVarVal i v f) (fmap (substVarVal i v) vs)
substVar i v (Fix fs e) =
    Fix
        (fmap (\(Fun d i as e)->Fun d i as (substVar i v e)) fs)
        (substVar i v e)
substVar i v (Record vs n e) =
    Record (fmap (first (substVarVal i v)) vs) n (substVar i v e)
substVar i v (Select ix va n e) =
    Select ix (substVarVal i v va) n (substVar i v e)
substVar i v (Switch x es) = Switch (substVarVal i v x) (fmap (substVar i v) es)
substVar i v (Primop p vs n es) =
    Primop p (fmap (substVarVal i v) vs) n (fmap (substVar i v) es)
substVar _ _ x = x

-- for inlining singly-called functions, no renaming is required
inlineSingle :: CFun -> CExp -> CExp
inlineSingle (Fun _ i as e) (App (Label j) xs) | i == j =
    foldr (uncurry substVar) e (zip as xs)
inlineSingle f (Fix fs e) =
    Fix (fmap (\(Fun d i as e) -> Fun d i as (inlineSingle f e)) fs)
        (inlineSingle f e)
inlineSingle f (Record vs i e) = Record vs i (inlineSingle f e)
inlineSingle f (Select i v n e) = Select i v n (inlineSingle f e)
inlineSingle f (Switch v es) = Switch v (fmap (inlineSingle f) es)
inlineSingle f (Primop p vs i es) = Primop p vs i (fmap (inlineSingle f) es)
inlineSingle _ x = x

dropDeadFns :: CExp -> CExp
dropDeadFns (Fix fs e) =
    let usedFns = filter (\(Fun d i _ s) -> issplit d || isexport d || usesOf i (Fix fs e) - usesOf i s /= 0) fs
    in if length usedFns == length fs then
        Fix fs e
    else
        dropDeadFns (Fix usedFns e)
dropDeadFns x = x

simpleInline :: CExp -> CExp
simpleInline e@(Fix fs _) =
    let toInline = filter (\(Fun d i _ s) -> not (issplit d) && not (isexport d) && usesOf i s == 0 && usesOf i e == 1) fs
    in if null toInline then
        e
    else
        simpleInline (dropDeadFns (inlineSingle (head toInline) e))