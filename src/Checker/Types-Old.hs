{-# LANGUAGE FunctionalDependencies #-}
module Checker.Types where

import Types.Kind
import Types.Type
import Types.Ident
import Types.Pattern
import Parser.Syntax
import qualified Checker.Kinds as Kinds

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

desugar :: Type -> Type
desugar (TypeApp (TypeApp FunctionType (SeqType [])) x) =
    let x' = desugar x in
        TypeApp LazyType x'
desugar (TypeApp (TypeApp FunctionType (SeqType xs@(_:_))) x) =
    let x' = desugar x
        xs' = map desugar xs in
        foldl (\a x -> TypeApp (TypeApp FunctionType x) (SeqType [a])) (TypeApp (TypeApp FunctionType (last xs')) x') (reverse (init xs'))
desugar (TypeApp a b) = TypeApp (desugar a) (desugar b)
desugar (SeqType xs) = SeqType (map desugar xs)
desugar x = x

arity :: Type -> Int
arity (TypeApp (TypeApp FunctionType _) o) = 1 + (arity o)
arity _ = 0

data Scheme = Forall [Name] Type deriving(Eq)

instance Show Scheme where
    show (Forall ns t) = "∀" ++ (ns >>= (\n -> ' ':n)) ++ ". " ++ show t

type TypeEnv = Map.Map Identifier Scheme

extend :: Identifier -> Scheme -> TypeEnv -> TypeEnv
extend = Map.insert

type Unique = [Name]

type InferState = (Unique, TypeEnv)

type Infer = ExceptT TypeError (State InferState)

type Subst = Map.Map Name Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set Name

instance Substitutable Type where
    apply s t@(TypeVar n) = Map.findWithDefault t n s
    apply s (TypeApp a b) = TypeApp (apply s a) (apply s b)
    apply s (SeqType xs) = SeqType (map (apply s) xs)
    apply _ t = t

    ftv (TypeVar n) = Set.singleton n
    ftv (TypeApp a b) = (ftv a) <> (ftv b)
    ftv (SeqType xs) = mconcat (map ftv xs)
    ftv _ = Set.empty

instance (Substitutable s, Ord k) => Substitutable (Map.Map k s) where
    apply s = fmap (apply s)

    ftv = foldr ((<>) . ftv) Set.empty

instance (Substitutable s) => Substitutable [s] where
    apply s = fmap (apply s)

    ftv = foldr ((<>) . ftv) Set.empty

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t

    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

names :: [Name]
names = [1..] >>= flip replicateM ['a'..'z']

execWithEnv :: (TypeEnv -> TypeEnv) -> Infer a -> Infer a
execWithEnv f m = do
    (_, old) <- get
    modify (second f)
    out <- m
    modify (second (const old))
    pure out

fresh :: Infer Type
fresh = do
    ((n:ns), e) <- get
    put (ns, e)
    pure (TypeVar n)

occursIn :: Name -> Type -> Bool
_ `occursIn` (TypeVar _) = False
n `occursIn` x = Set.member n (ftv x)

(~~) :: Type -> Type -> Infer Subst
(TypeVar n) ~~ b = bind n b
a ~~ (TypeVar n) = bind n a
(TypeApp a b) ~~ (TypeApp x y) = do
    s0 <- a ~~ x
    s1 <- b ~~ y
    pure (s0 `compose` s1)
(SeqType as) ~~ (SeqType xs)
    | length as == length xs = do
        ss <- zipWithM (~~) as xs
        pure (foldl compose nullSubst ss)
    | otherwise = throwError (UnificationMismatch as xs)
a ~~ b | a == b = pure nullSubst
a ~~ b = throwError (UnificationFail a b)

bind :: Name -> Type -> Infer Subst
bind n t
    | n `occursIn` t = throwError (InfiniteType n t)
    | otherwise = pure (Map.singleton n t)

instantiate :: Scheme -> Infer Type
instantiate (Forall ns t) = do
    ns' <- mapM (const fresh) ns
    let s = Map.fromList (zip ns ns')
    pure (apply s t)

generalize :: TypeEnv -> Type -> Scheme
generalize e t = Forall (Set.toList (ftv t `Set.difference` ftv e)) t

lookupEnv :: Identifier -> Infer Type
lookupEnv id = do
    (_, env) <- get
    case Map.lookup id env of
        Nothing -> throwError (UnboundIdent id)
        Just s -> instantiate s

data TaggedStmts = Stmts [Tagged TaggedStmt] TypeStack deriving(Eq, Show)
data TaggedPattern = Pat [Pattern] TypeStack deriving(Eq, Show)

data TaggedStmt
    = TPush TaggedExpr
    | TExec TaggedExpr
    | TLet Name TaggedStmts
    deriving(Eq, Show)

data TaggedExpr
    = TName Identifier
    | TLam [Tagged Name] TaggedStmts
    | TCase [(TaggedPattern, TaggedStmts)]
    deriving(Eq, Show)

inferExpr :: Expr -> Infer (Subst, Type, TaggedExpr)
inferExpr exp = case exp of
    ENam ident -> do
        t <- lookupEnv ident
        pure (nullSubst, t, TName ident)

    ELam (Lambda ns stmts) -> do
        nmap <- mapM (\n -> (\t -> (LocalIdentifier n, Forall [] t)) <$> fresh) ns
        (s0, ts, tags) <- execWithEnv (Map.union (Map.fromList nmap)) (infer stmts)
        let targs = map (uncurry (flip Tagged)) nmap
            tstmts = Stmts tags ts
        pure (s0, SeqType ts, TLam targs tstmts)
    
    ECse (Case us) -> do
        (s0, us') <- foldM (\(s0, ps) (pts, stmts) -> do
            (pts, p) <- inferPattern pts
            (s1, tst, tstmts) <- infer stmts
            pure (s0 `compose` s1, (Pat p pts, Stmts ):ps)) (nullSubst, []) us
        pure (s0, undefined, undefined) -- need to create actual return type

{-
data Tagged a
    = Tagged a Type
    deriving(Show, Eq)

instance Substitutable (Tagged t) where
    apply s (Tagged a t) = Tagged a (apply s t)
    ftv (Tagged _ t) = ftv t
-}
type TypeStack = [Type]

data Tagged a
    = Tagged a Type
    deriving(Eq)

instance (Show a) => Show (Tagged a) where
    show (Tagged a t) = (show a) ++ " :: " ++ (show t)

instance Substitutable (Tagged a) where
    apply s (Tagged a t) = Tagged a (apply s t)
    ftv (Tagged _ t) = ftv t

data Action a
    = Push (Tagged a)
    | Exec (Tagged a)
    | None (Tagged a)
    deriving(Eq, Show)

class Inferable i a | i -> a where
    inferSingle :: i -> Infer (Subst, Action a)

    infer :: [i] -> Infer (Subst, TypeStack, [Tagged a])
    infer is = internal is []
        where
            exec :: Type -> TypeStack -> Infer (Subst, TypeStack)
            exec t (u:ts) = case t of
                TypeApp (TypeApp FunctionType i) o -> do
                    s0 <- i ~~ u
                    (s1, ts') <- exec o ts
                    pure (s0 `compose` s1, ts')
                SeqType xs -> pure (nullSubst, xs ++ ts)
                _ -> pure (nullSubst, t:u:ts)

            internal :: (Inferable i a) => [i] -> TypeStack -> Infer (Subst, TypeStack, [Tagged a])
            internal (i:is) ts = do
                (s0, a) <- inferSingle i
                case a of
                    Push tag@(Tagged _ t) -> do
                        (s1, ts', tags) <- internal is (t:(apply s0 ts))
                        pure (s1 `compose` s0, ts', tag:tags)
                    Exec tag@(Tagged _ t) -> do
                        (s1, ts') <- exec t (apply s0 ts)
                        (s2, ts'', tags) <- internal is (apply s1 ts')
                        pure (s2 `compose` s1 `compose` s0, ts'', tag:tags)
                    None -> do
                        (s1, ts', tags) <- internal is (apply s0 ts)
                        pure (s1 `compose` s0, ts', tags)

makePattern :: [Tagged Pattern] -> Infer [Pattern]
makePattern = internal [] . reverse
    where
        internal :: [Pattern] -> [Tagged Pattern] -> Infer [Pattern]
        internal = foldM internalSingle

        internalSingle :: [Pattern] -> Tagged Pattern -> Infer [Pattern]
        internalSingle pstack (Tagged p@(PatternCons _) t)
            | length pstack >= arity t =
                let n = arity t
                    args = take n pstack
                    rem = drop n pstack
                    pat = foldl PatternApp p args in
                        pure (pat:rem)
            | otherwise = throwError (PatternUnderflow p pstack)
        internalSingle pstack (Tagged p _) = pure (p:pstack)

inferPattern :: [PatternTok] -> Infer (TypeStack, [Pattern])
inferPattern pt = do
    (_, typ, tags) <- infer pt
    p <- makePattern tags
    pure (typ, tags)

instance Inferable PatternTok Pattern where
    inferSingle (PNam id) = do
        t <- lookupEnv id
        pure (nullSubst, Exec (Tagged (PatternCons id) t))
    inferSingle (PVar n) = do
        t <- fresh
        pure (nullSubst, Push (Tagged (PatternVar n) t))
    inferSingle PAny = do
        t <- fresh
        pure (nullSubst, Push (Tagged PatternWildcard t))

inferStmt :: Stmt -> Infer (Subst, Action Stmt)
inferStmt stmt = case stmt of
    SExp x -> do
        (s0, t, tags) <- inferExpr x
        pure (s0, Exec (Tagged stmt t))
    SPsh x -> do
        (s0, t, tags) <- inferExpr x
        pure (s0, Push (Tagged stmt t))
    
    --SLet (Case us) ->

instance Inferable Stmt Stmt where
    inferSingle = inferStmt

{-
replace :: Name -> Type -> Type -> Type
replace n t (TypeVar n0)
    | n == n0 = t
    | otherwise = TypeVar n0
replace n t (TypeApp a b) = TypeApp (replace n t a) (replace n t b)
replace n t (SeqType xs) = SeqType (map (replace n t) xs)
replace _ _ x = x

substitute :: Subst -> Type -> Type
substitute env (TypeVar n) = case Map.lookup n env of
    Just t -> t
    Nothing -> TypeVar n
substitute env (TypeApp a b) = TypeApp (substitute env a) (substitute env b)
substitute env (SeqType xs) = SeqType (map (substitute env) xs)
substitute _ x = x

type TypeStack = [Type]

push :: Type -> TypeStack -> TypeStack
push = (:)

fresh :: Infer Name
fresh = do
    (n:ns) <- get
    put ns
    pure n
-}