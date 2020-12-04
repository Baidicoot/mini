module Glue where
-- Glue: hiding things from Main

import Text.Parsec
import Parser.SExpr
import Parser.Parser
import CPS.CPSify
import Elaborate.Elaborate
import Modules.Defunctorize
import TypeCheck.Check
import CPS.ClosureConv
import CPS.Spill
import CPS.Meta
import Backend.AbstGen

import Types.Module
import Types.Pretty
import Types.Ident
import Types.Core ( untagCore )
import Control.Monad.Errors ( toEither )
import Control.Monad.Except

import Error.Error ( render )

import System.IO

import Backend.X86_64_linux.Textual
import Backend.Backend

import qualified Data.Map as Map

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

compileStr :: Config -> String -> Repl ModuleAPI
compileStr config s = do
    let modulePath = ["Repl"]
    a <- case parse (many rpncc) "Repl" s of
        Right a -> pure a
        Left es -> throwError [show es]
    b <- toplevelexpr a
        `handleEither`
        (throwError . fmap (render s))
    (c,constructors,tlns,s0,w) <- toEither (elaborate 0 emptyServer emptyEnv modulePath b)
        `handleEither`
        (\(as,bs) -> throwError $ fmap (render s) as ++ fmap (render s) bs)
    liftIO $ prettyPrint c (0::Int)
    (d,tlsc,s1) <- toEither (typecheck s0 emptyServer c)
        `handleEither`
        (throwError . fmap (render s))
    liftIO $ prettyPrint d (0::Int)
    let (s2, e) = defunctorize (ExternalIdentifier ["Repl"] "main") [] s1 d
    let (f, (s3,_)) = cpsify [] emptyServer (untagCore e) s2
    liftIO $ putStrLn "\n\nCPS Converted:"
    liftIO $ prettyPrint f (0::Int)
    liftIO $ mapM (\(i,v) -> putStr (show i ++ ": ") >> print v) (Map.toList . fst $ collectPerFunctionMeta f)
    let (g,s4) = closureConv f s3
    let (h,s5) = spill (regs config) s4 g
    liftIO $ putStrLn "\n\nSpilled & Closure Converted:"
    liftIO $ prettyPrint h (0::Int)
    let i = generateAbstract [] (ExternalIdentifier ["Repl"] "main") h (regs config)
    liftIO $ putStrLn "\n\nAbstract:"
    liftIO $ print i
    let j = emit (codegen i :: X86 ())
    liftIO $ putStrLn j
    pure (ModuleAPI mempty mempty mempty mempty)