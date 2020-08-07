module Main where

import Parser.Parser

import Parser.SExpr

import Text.Parsec (parse)

import Types.Syntax
import Types.Type
import Types.Ident

import qualified Data.Map as Map

import Frontend.IRify


a = let (Right exp) = parse rpncc "" "(let ((:: fst (a -> b -> a)) (x y) x) (flip (f x y) (f y x)) (snd () (flip fst)) (fst snd flip))" in exp
b = let (Right exp) = runParse (parseexpr a) in exp

main = pure ()