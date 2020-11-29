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

gatherExterns :: Core Type -> [Scheme]
gatherExterns e = case getTag e of
    Node _ (Product ts) -> fmap (\t -> Forall (ftv t) t) ts
    _ -> []