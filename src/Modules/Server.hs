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

moduleSign :: Module -> Type
moduleSign = getSignature . moduleExpo