module Builtin.Decls where

import Types.Type
import Types.Ident
import Types.Graph

type Decl = (Identifier, Type)

errormsg :: Type
errormsg = Node () (ExternalIdentifier [] "Error")

matcherr :: Decl
matcherr = (ExternalIdentifier [] "Match", errormsg)

raise :: Decl
raise = (ExternalIdentifier [] "raise", Forall (Set.fromList ["a"]) $ errormsg -> (Node () (LocalIdentifier "a")))