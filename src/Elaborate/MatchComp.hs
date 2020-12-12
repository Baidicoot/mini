{-# LANGUAGE TupleSections #-}

module Elaborate.MatchComp (matchcomp) where

import Elaborate.Elaborator

import Types.Pattern
import Types.Core
import Types.Prim
import Types.Ident
import Types.Graph

import Control.Arrow (first, (***))
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

finish :: ClauseMatrix -> Elaborator (Core SourcePos)
finish ((_,m,a):_,_) = a m

empty :: ClauseMatrix -> Bool
empty ([],_) = True
empty _ = False

conspat :: ClauseMatrix -> Bool
conspat ([],_) = True
conspat ((PatternCons _ _ _:_,_,_):xs,a) = conspat (xs,a)
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

matchhead :: Identifier -> [Identifier] -> ClauseMatrix -> ClauseMatrix
matchhead i ns' (xs,_:ns) = (internal xs, ns'++ns)
    where
        internal ((PatternCons _ j ss:ps,r,a):xs)
            | j == i && length ss == length ns' = (ss++ps,r,a):internal xs
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

getvar :: ClauseMatrix -> Identifier
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
cut (xs@((PatternCons _ _ _:_,_,_):_),ns) = ((,ns) *** (,ns)) (span isCons xs)
cut (xs@((PatternLit _ _:_,_,_):_),ns) = ((,ns) *** (,ns)) (span isLit xs)
cut (xs@((PatternVar _ _:_,_,_):_),ns) = ((,ns) *** (,ns)) (span isVar xs)
cut (xs@((PatternWildcard _:_,_,_):_),ns) = ((,ns) *** (,ns)) (span isVar xs)

compileDefaults :: SourcePos -> [ClauseMatrix] -> Elaborator (Core SourcePos)
compileDefaults p [] = pure . Node p $ Error ("incomplete pattern at " ++ show p)
compileDefaults p (x:xs) = matchcomp p x xs

matchcomp :: SourcePos -> ClauseMatrix -> [ClauseMatrix] -> Elaborator (Core SourcePos)
matchcomp p c ds
    | empty c = pure . Node p $ Error ("empty pattern at " ++ show p)
    | finished c = finish c
    | conspat c = do
        let cons = constructors c
        cses <- flip mapM cons $ \(i,n) -> do
            ns' <- replicateM n fresh
            let c' = matchhead i (fmap LocalIdentifier ns') c
            exp <- matchcomp p c' ds
            pure (ConsPattern i ns', p, exp)
        dflt <- compileDefaults p ds
        pure . Node p $ Match (Nothing,p) (getvar c) (cses ++ [(WildcardPattern, p, dflt)])
    | litpat c = do
        let lits = literals c
        cses <- flip mapM lits $ \l -> do
            let c' = matchlit l c
            exp <- matchcomp p c' ds
            pure (LiteralPattern l, p, exp)
        dflt <- compileDefaults p ds
        pure . Node p $ Match (Nothing,p) (getvar c) (cses ++ [(WildcardPattern, p, dflt)])
    | varpat c = matchcomp p (matchvar c) ds
    | otherwise = let (c1, c2) = cut c in matchcomp p c1 (c2:ds)