module Types.Ident where
import Data.List (intercalate)
import Control.Monad (replicateM)

type Name = String

type ModulePath = [Name]

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
    show (ExternalIdentifier ms n) = intercalate "." ms ++ "." ++ n
    show (LocalIdentifier n) = n