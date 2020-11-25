module Elaborate.Defunctorize (defunctorize) where

import Types.Core
import Types.Type
import Types.Env
import Types.Ident
import Types.Graph
import Types.Prim

fresh :: Int -> (Int,Name)
fresh i = (i+1,'i':show i)

doImportAs :: Int -> Name -> ModuleExports -> Module -> [Name] -> Core Type -> (Int,Core Type)
doImportAs s f m m' ts e =
    let importing = filter ((`notElem` ts) . fst . fst) $ zip (termTypes m) [0..]
    in (,) s
        $ foldr (\((n,Forall _ t),i) ->
            Node (getTag e)
            . Let (ExternalIdentifier m' n) (Node t . Select i . Var $ LocalIdentifier f)) e importing

defunctorizeArg :: [(Name,Type)] -> [(ModuleExports,ImportAction)] -> Int -> Core Type -> (Int,Core Type)
defunctorizeArg ns ((m,ImportAsHiding m' t _):xs) s e =
    let (s',f) = fresh s in uncurry (defunctorizeArg ((f,getSignature m):ns) xs) (doImportAs s' f m m' t e)
defunctorizeArg ns [] s e =
    let (s',f) = fresh s
        (s'',a) = fresh s'
        e' = foldr (\((n,t),i) -> 
            Node (getTag e)
            . Let (LocalIdentifier n) (Node t . Select i . Var $ LocalIdentifier f)) e (zip (reverse ns) [0..])
    in (,) s''
        . Node (Node NoTag $ Builtin UnitTy)
        . Fix [(LocalIdentifier f,Node (getTag e) $ Lam (LocalIdentifier a) e')]
        $ Node (Node NoTag $ Builtin UnitTy) . Val $ Lit Unit

defunctorize :: [(ModuleExports,ImportAction)] -> Int -> Core Type -> (Int,Core Type)
defunctorize = defunctorizeArg []