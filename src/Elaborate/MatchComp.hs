{-# LANGUAGE TupleSections #-}

module Elaborate.MatchComp (matchcomp) where

import Elaborate.Elaborator

import Types.Pattern
import Types.Core
import Types.Prim
import Types.Ident
import Types.Graph

import Control.Arrow (first)
import Control.Monad
import Control.Monad.RWS

import Text.Parsec.Pos
import qualified Data.Map as Map

-- algorithm here is as described in https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching/
-- however, I generalise their useage of exceptions to one of defaults
-- to make up for the lack of exceptions in this language

finished :: ClauseMatrix -> Bool
finished (([],_,_):_,_) = True
finished _ = False

conspat :: ClauseMatrix -> Bool
conspat ([],_) = True
conspat ((PatternCons _ _ _:_,_,_):xs,_) = conspat (xs,a)
conspat _ = False

varpat :: ClauseMatrix -> Bool
varpat ((PatternVar _ _:_,_,_):xs,a) = varpat (xs,a)
varpat ((PatternWildcard _:_,_,_):xs,a) = varpat (xs,a)
varpat ([],_) = True
varpat _ = False

litpat :: ClauseMatrix -> Bool
litpat ([],_) = True
litpat ((PatternLit _ _:_,_,_):xs,a) = litpat (xs,a)
litpat _ = False

constructors :: ClauseMatrix -> [(Identifier, Int)]
constructors (xs,_) = internal [] xs
    where
        internal is ((PatternCons _ i s:_,_,_):xs)
            | not (i `elem` (fmap fst is)) = internal (is ++ [(i,length s)]) xs
            | otherwise = internal is xs
        internal is (_:xs) = internal is xs
        internal is [] = is

literals :: ClauseMatrix -> [UnboxedLit]
literals (xs,_) = internal [] xs
    where
        internal ls ((PatternLit _ l:_,_,_):xs)
            | not (l `elem` ls) = internal (ls ++ [l]) xs
            | otherwise = internal ls xs
        internal ls (_:xs) = internal ls xs
        internal ls [] = ls

matchhead :: Identifier -> [Name] -> ClauseMatrix -> ClauseMatrix
matchhead i ns' (xs,_:ns) = (internal xs, ns'++ns)
    where
        internal ((PatternCons _ j ss:ps,r,a):xs)
            | j == i && length ss == n = (ss++ps,r,a):internal xs
        internal (_:xs) = internal xs
        internal [] = []

matchvar :: ClauseMatrix -> ClauseMatrix
matchvar (xs,n':ns) = (internal xs,ns)
    where
        internal ((PatternWildcard _:ps,r,a):xs) = (ps,r,a):internal xs
        internal ((PatternVar _ n:ps,r,a):xs) = (ps,Map.insert n n' r,a):internal xs
        internal _ = []

matchlit :: UnboxedLit -> ClauseMatrix -> ClauseMatrix
matchlit l (xs,_:ns) = (internal xs,ns)
    where
        internal ((PatternLit _ m:ps,r,a):xs)
            | m == l = (ps,r,a):internal xs
        internal (_:xs) = internal xs
        internal [] = []

getvar :: ClauseMatrix -> Name
getvar (_,n:_) = n

isCons :: ClauseRow -> Bool
isCons (PatternCons _ _ _:_,_,_) = True
isCons _ = False

isLit :: ClauseRow -> Bool
isLit (PatternLit _ _:_,_,_) = True
isLit _ = False

isVar :: ClauseRow -> Bool
isVar (PatternVar _ _:_,_,_) = True
isVar (PatternWildcard _:_,_,_) = True
isVar _ = False

cut :: ClauseMatrix -> (ClauseMatrix, ClauseMatrix)
cut (xs@((PatternCons _ _ _:_,_,_):_),ns) = fmap ((,ns) *** (,ns)) (span isCons xs)
cut (xs@((PatternLit _ _:_,_,_):_),ns) = fmap ((,ns) *** (,ns)) (span isLit xs)
cut (xs@((PatternVar _ _:_,_,_):_),ns) = fmap ((,ns) *** (,ns)) (span isVar xs)
cut (xs@((PatternWildcard _:_,_,_):_),ns) = fmap ((,ns) *** (,ns)) (span isVar xs)

compileToCases :: SourcePos -> ClauseMatrix -> Elaborator [(PatternBinding, SourcePos, Core SourcePos)]
compileToCases p c =
    if finished c then do
        exp <- finish c
        pure [(PatternWildcard, p, exp)]
    else if conspat c then do
        let cons = constructors c
        flip mapM cons $ \(i,n) -> do
            ns' <- replicateM n fresh
            let c' = matchhead i ns' c
            exp <- matchcomp p c'
            pure (ConsPattern i ns', p, exp)
    else if litpat c then do
        let lits = literals c
        flip mapM lits $ \l -> do
            let c' = matchlit l c
            exp <- matchcomp p c'
            pure (LiteralPattern l, p, exp)
    else if varpat c then
        compileToCases p (matchvar c)
    else do
        let (c1, c2) = cut c
        cses <- compileToCases p c
        dflt <- matchcomp p c2
        pure (cses ++ [(PatternWildcard, p, dflt)])

matchcomp :: SourcePos -> ClauseMatrix -> Elaborator (Core SourcePos)
matchcomp p c =
    if finished c then
        finish c
    else do
        let n = getvar c
        cses <- compileToCases p c
        pure (Node p $ Match n cses)