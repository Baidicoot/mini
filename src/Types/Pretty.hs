{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Pretty where

class Pretty t d where
    showtag :: t -> d -> Bool
    pretty :: t -> d -> String

prettyPrint :: (Pretty t d) => t -> d -> IO ()
prettyPrint t d = putStrLn (pretty t d)