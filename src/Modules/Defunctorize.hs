module Modules.Defunctorize (defunctorize) where

import Types.Core
import Types.Type
import Types.Module
import Types.Ident
import Types.Graph
import Types.Prim

fresh :: Int -> (Int,Name)
fresh i = (i+1,'i':show i)

doImportAs :: Int -> Name -> ModuleAPI -> ModulePath -> Core Type -> (Int,Core Type)
doImportAs s f m m' e =
    let importing = zip (moduleAPITerms m) [0..]
    in (,) s
        $ foldr (\((n,Forall _ t),i) ->
            Node (getTag e)
            . Let (ExternalIdentifier m' n) (Node t . Select i . Var $ LocalIdentifier f)) e importing

defunctorizeArg :: Identifier -> Name -> [(Name,Type)] -> [(ModuleAPI,ImportAction)] -> Int -> Core Type -> (Int,Core Type)
defunctorizeArg i a ns ((m,ImportAs m'):xs) s e =
    let (s',f) = fresh s in uncurry (defunctorizeArg i a ((f,getSignature m):ns) xs) (doImportAs s' f m m' e)
defunctorizeArg i a ns [] s e =
    let e' = foldr (\((n,t),i) -> 
            Node (getTag e)
            . Let (LocalIdentifier n) (Node t . Select i . Var $ LocalIdentifier a)) e (zip (reverse ns) [0..])
    in (,) s
        . Node unitty
        . Fix [(i,Node (getTag e) $ Lam (LocalIdentifier a) e')]
        $ Node unitty $ Tuple []

defunctorize :: Identifier -> [(ModuleAPI,ImportAction)] -> Int -> Core Type -> (Int,Core Type)
defunctorize i m s = let (s',a) = fresh s in defunctorizeArg i a [] m s'