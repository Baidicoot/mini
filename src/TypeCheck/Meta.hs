module TypeCheck.Meta (gatherExterns) where

import Types.Env
import Types.Ident
import Types.Core
import Types.Type
import Types.Graph

import Control.Arrow ((***))

isExtern :: Identifier -> Bool
isExtern (ExternalIdentifier _ _) = True
isExtern _ = False

local :: Identifier -> Name
local (ExternalIdentifier _ n) = n
local (LocalIdentifier n) = n

gatherExternDefs :: Core Type -> [(Name, Scheme)]
gatherExternDefs (Node _ (Fix fs x)) =
    ((local *** ((\t -> Forall (ftv t) t) . getTag)) <$> filter (isExtern . fst) fs)
    ++ gatherExternDefs x
gatherExternDefs _ = []

gatherExterns :: Core Type -> ModuleExports
gatherExterns e = let tys = gatherExternDefs e in mempty
    { termNames = fmap fst tys
    , termTypes = tys
    }