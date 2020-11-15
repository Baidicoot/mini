module Backend.Backend where

import Types.Abstract

class Backend b where
    codegen :: [Operator] -> b
    emit :: b -> String