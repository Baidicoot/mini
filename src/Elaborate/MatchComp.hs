module Frontend.MatchComp (MatchWarning(..), matchcomp) where

import Types.Pattern
import Types.Core
import Types.Prim

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map

mkBind :: [Name] -> PatternConstructor -> PatternBinding
mkBind ns (ConsCons id i) = ConsPattern id (take i ns)
mkBind _ (ConsLit l) = LiteralPattern l
mkBind _ ConsWild = WildcardPattern

type Call tag = (Identifier, [Name], tag)
type UncompiledBranch tag = ([(Name, Pattern tag)], Call tag, Map.Map Name Name)

pseudoLookup :: Name -> [(Name, a)] -> Maybe a
pseudoLookup n ((o, a):_)
    | n == o = Just a
pseudoLookup n (_:xs) = pseudoLookup n xs
pseudoLookup n [] = Nothing

pseudoDelete :: Name -> [(Name, a)] -> [(Name, a)]
pseudoDelete n = filter ((/=n) . fst)

finished :: UncompiledBranch tag -> Bool
finished ([],_,_) = True
finished _ = False

finish :: UncompiledBranch tag -> Core tag
finish (_,(n,[],t),_) = App t (Node t . Val $ Var n) (Node t . Val $ Lit Unit)
finish (_,(n,as,t),r) = foldl (\b a -> App t b
    ( Node t . Val . Var
    . LocalIdentifier
    $ Map.findWithDefault n n r)) (Node t . Val $ Var n) as

argsOf :: Name -> [(Name, Pattern tag)] -> [Pattern tag]
argsOf n m = case pseudoLookup n m of
    Just x  -> pargs x
    Nothing -> []

matchOn :: Name -> UncompiledBranch tag -> PatternConstructor
matchOn n (m, _, _) = case pseudoLookup n m of
    Just x  -> cons x
    Nothing -> ConsWild

data MatchWarning tag
    = Unreachable [UncompiledBranch tag]
    | Incomplete
    deriving(Eq, Show)

type MatchCompiler tag = StateT Int (Writer [MatchWarning tag])

runMatchCompiler :: Int -> MatchCompiler tag a -> ((a, Int), [MatchWarning tag])
runMatchCompiler i = runWriter . runStateT i

fresh :: MatchCompiler Name
fresh = do
    n <- get
    put (n+1)
    pure ("m" ++ show n)

group :: Name -> [UncompiledBranch tag] -> [(PatternConstructor, [UncompiledBranch tag])]
group n (x:xs) =
    let p = matchOn n x
        s = filter ((`fits` p) . matchOn n) xs
        f = filter ((/=p) . matchOn n) xs
    in (p, x:s):group f
group _ [] = []

deconstruct :: Name -> [Name] -> UncompiledBranch tag -> UncompiledBranch tag
deconstruct n ns (m,c,r) =
    let ps = zip ns $ argsOf n m
        m' = pseudoDelete n m
        (ps', r') = genRenamings r ps
    in (m' ++ ps',c,r')
    where
        genRenamings r ((n,PatternVar o):xs) = genRenamings (Map.insert o n r) xs
        genRenamings r ((_,PatternWildcard):xs) = genRenamings r xs
        genRenamings r (x:xs) = first (x:) (genRenamings r xs)
        genRenamings r [] = ([], r)

selectvar :: [UncompiledBranch tag] -> Name
selectvar ((n,_):_,_,_) = n

compileMatch :: tag -> [UncompiledBranch tag] -> MatchComp (Core tag)
compileMatch t [] = tell [Incomplete] >> pure (Node t . Error $ "MatchError: partial match at " ++ show t)
compileMatch t (x:xs)
    | finished x = tell [Unreachable xs] >> pure (finish xs)
compileMatch t bs = do
    let n = selectvar bs
    let gs = group n bs
    gs' <- mapM (\(p,bs) -> do
        argns <- replicateM (len p) fresh
        let bs' = fmap (deconstruct n argns) bs
        tree <- compileMatch t bs'
        pure (mkBind p, t, tree)) gs
    pure (Node t $ Match n gs')

matchcomp :: Int -> Name -> tag -> [(Pattern tag, Call tag)] -> (Core tag, Int, [MatchWarning tag])
matchcomp i n t ps =
    let ps' = fmap (\(p,c) -> ([(n,p)],c,mempty)) ps
        ((exp, i'), warnings) = runMatchCompiler i (matchCompile t ps')
    in (exp, i', warnings)