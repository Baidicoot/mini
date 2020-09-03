module Builtin.Decls where

import Types.Type
import Types.Ident
import Types.Graph

import qualified Data.Set as Set

type Decl = (Identifier, Scheme)

comp :: Name -> Identifier
comp = ExternalIdentifier ["Arc"]

errormsg :: Type
errormsg = Node () (NamedType $ comp "Error")

matcherr :: Decl
matcherr = (comp "Match", Forall Set.empty errormsg)

unitty :: Type
unitty = Node () (NamedType $ comp "Unit")

unit :: Decl
unit = (comp "Unit", Forall Set.empty unitty)

raise :: Decl
raise = (comp "raise", Forall (Set.fromList ["a"]) $ errormsg --> (Node () (TypeVar "a")))