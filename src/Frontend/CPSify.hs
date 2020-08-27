module Frontend.CPSify where

import Types.CPS
import qualified Types.IR as IR
import qualified Types.Graph as Graph

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow
import Control.Monad

type CPSState = ([Name], [Name])

fresh :: CPSifier Name
fresh = do
    ((n:ns), ks) <- get
    put (ns, ks)
    pure n

cont :: CPSifier Name
cont = do
    (ns, (k:ks)) <- get
    put (ns, ks)
    pure k

type CPSifier = State CPSState

convertNode :: IR.PolyIRNode Type tag -> (Value -> CExp) -> CPSifier CExp
convertNode (IR.Var id) c = pure $ c (Var id)
convertNode (IR.Unboxed u) = pure $ c (Unboxed u)
convertNode (IR.Lam v e) = do
    f <- fresh
    k <- cont
    bigF <- convert e (\z -> App (Var (LocalIdentifier k)) [z])
    pure $ Fix [Fun (LocalIdentifier f) [v, k] bigF] (c (Var (LocalIdentifier f)))

convert :: IR.PolyIR Type tag -> (Value -> CExp) -> CPSifier CExp