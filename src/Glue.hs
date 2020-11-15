module Glue where
-- Glue: hiding things from Main

import Text.Parsec
import Parser.SExpr
import Parser.Parser
import CPS.CPSify
import Elaborate.Elaborate
import TypeCheck.Check
import CPS.ClosureConv
import CPS.Spill
import Backend.AbstGen

import Types.Env
import Types.Pretty
import Types.Core ( untagCore )
import Control.Monad.Errors ( toEither )
import Control.Monad.Except

import Error.Error ( render )

import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

type Repl = ExceptT [String] IO
newtype Config = Config {regs :: Int}

handleEither :: Either a b -> (a -> Repl b) -> Repl b
handleEither (Right b) f = pure b
handleEither (Left a) f = f a

compileStr :: Config -> String -> Repl ModuleExports
compileStr config s = do
    let modulePath = ["Repl"]
    let exports = mempty {moduleMod = modulePath}
    a <- case parse (many rpncc) "Repl" s of
        Right a -> pure a
        Left es -> throwError [show es]
    b <- toplevelexpr a
        `handleEither`
        (throwError . fmap (render s))
    (c,constructors,s0,w) <- toEither (elaborate 0 modulePath mempty b)
        `handleEither`
        (\(as,bs) -> throwError $ fmap (render s) as ++ fmap (render s) bs)
    let exports' = exports `mappend` constructors
    (d,functions,s1) <- toEither (typecheck s0 (importWithAction include exports') c)
        `handleEither`
        (throwError . fmap (render s))
    liftIO $ prettyPrint c (0::Int)
    liftIO $ prettyPrint d (0::Int)
    let exports'' = exports' `mappend` functions
    let (e, s2) = cpsify (importWithAction include exports'') (untagCore d) s1
    liftIO $ putStrLn "\n\nCPS Converted:"
    liftIO $ prettyPrint e (0::Int)
    let (f,s3) = closureConvert e s2
    let (g,s4) = spill (regs config) s3 f
    let h = generateAbstract g (regs config)
    liftIO $ putStrLn "\n\nAbstract:"
    liftIO $ print h
    pure exports''