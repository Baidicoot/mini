module Backend.AbstGen where

import Types.Abstract
import Types.Ident
import Types.CPS
import qualified Types.Bijection as Bij

import Prelude hiding(drop)
import Control.Monad
import Control.Monad.RWST

import qualified Data.Map as Map

type AbstState = Map.Map Name [GPR]
type AbstEnv = (Bij.Bijection Name GPR, Set.Set Name)

type AbstGen = RWS AbstEnv [Operator] AbstState

-- observations:
-- if a called function is known, it is non-escaping
-- if a called function is unknown, it is escaping
-- escaping functions use r0 for first argument, etc.

l1 :: (Monad m) => (o -> m p) -> (a -> o) -> a -> m p
l1 m f = m . f

l2 :: (Monad m) => (o -> m p) -> (a -> b -> o) -> a -> b -> m p
l2 m f = ((.) . (.)) m f

l3 :: (Monad m) => (o -> m p) -> (a -> b -> c -> o) -> a -> b -> c -> m p
l3 m f a b c = m (f a b c)

emit :: Operator -> AbstGen ()
emit op = tell [op]

emitlit = l2 emit EmitLit
emitptr = l3 emit EmitPtr
define = l1 emit Define
check = l1 emit CheckLim
jmp = l1 emit Jmp
record = l2 emit Record
select = l3 emit Select
move = l2 emit Move

store :: Name -> GPR -> AbstGen a -> AbstGen a
store n r = local (\(m,e) -> (Bij.insert n r m, e))

clear :: GPR -> AbstGen a -> AbstGen a
clear r = local (\(m,e) -> (Bij.deleteSnd r m, e))

drop :: Name -> AbstGen a -> AbstGen a
drop n = local (\(m,e) -> (Bij.deleteFst n m, e))

getReg :: Name -> AbstGen (Maybe GPR)
getReg n = do
    (m,_) <- ask
    pure . fmap snd $ Bij.lookupFst n m

getVar :: GPR -> AbstGen (Maybe Name)
getVar r = do
    (m,_) <- ask
    pure . fmap snd $ Bij.lookupSnd r m

known :: Name -> AbstGen Bool
known n = do
    (_,e) <- ask
    pure $ Set.member n e

getRegLayout :: Name -> AbstGen (Maybe [GPR])
getRegLayout n = do
    e <- get
    pure $ Map.lookup n e

swap :: GPR -> GPR -> AbstGen a -> AbstGen a
swap a b = do
    move ar (r a)
    move (r a) (r b)
    move (r b) ar