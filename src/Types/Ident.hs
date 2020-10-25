module Types.Ident where
import Data.List (intercalate)
import Control.Monad (replicateM)

type Name = String

type Module = [Name]

data Identifier
    = ExternalIdentifier Module Name
    | LocalIdentifier Name
    deriving(Eq, Ord)

extractLocals :: [Identifier] -> [Name]
extractLocals (LocalIdentifier n:ns) = n:extractLocals ns
extractLocals (_:xs) = extractLocals xs
extractLocals [] = []

instance Show Identifier where
    show (ExternalIdentifier ms n) = intercalate "." ms ++ "." ++ n
    show (LocalIdentifier n) = n