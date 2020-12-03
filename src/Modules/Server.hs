module Modules.Server where

import Types.Ident
import Types.Env
import Types.Abstract
import Types.Type

data Module = Module
    { modulePath :: ModulePath
    , moduleFunc :: Identifier
    , moduleRegs :: [GPR]
    , moduleExpo :: ModuleExports
    , moduleReqs :: [ModulePath]
    } deriving(Show)

type ModuleServer = [Module]

-- notes about environments:

-- required for cps conversion:
--      * list of known functions
--      * variadic identifier -> tag information
--      * variadic aritiy information

-- required for closure conversion:

-- required for register spilling:
--      * number of general-purpose registers

-- required for abstract machine generation:
--      * list of escaping functions

-- required for elaboration:
--      * current module name
--      * imported term renames
--      * imported type renames
--      * variadic aritiy information

-- required for defunctorisation:
--      * all term import name information

-- required for typechecking:
--      * all term import type information
--      * all type import kind information

moduleSign :: Module -> Type
moduleSign = getSignature . moduleExpo