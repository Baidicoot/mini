module Types.Ident where
import Data.List (intercalate)
import Control.Monad (replicateM)

data Name
    = User String
    | Gen String Int
    | Symb String
    deriving(Eq,Ord)

instance Show Name where
    show (User s) = s
    show (Gen s n) = s ++ show n
    show (Symb s) = s

type ModulePath = [String]

data Identifier
    = ExternalIdentifier ModulePath Name
    | LocalIdentifier Name
    deriving(Eq, Ord)

extractLocals :: [Identifier] -> [Name]
extractLocals (LocalIdentifier n:ns) = n:extractLocals ns
extractLocals (_:xs) = extractLocals xs
extractLocals [] = []

discardPath :: Identifier -> Name
discardPath (LocalIdentifier n) = n
discardPath (ExternalIdentifier p n) = n

instance Show Identifier where
    show (ExternalIdentifier ms n) = intercalate "." ms ++ "." ++ show n
    show (LocalIdentifier n) = show n