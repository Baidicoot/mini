module Types.Ident where
import Data.List (intercalate)

type Name = String

type Module = [Name]

data Identifier
    = ExternalIdentifier Module Name
    | LocalIdentifier Name
    deriving(Eq, Ord)

instance Show Identifier where
    show (ExternalIdentifier ms n) = intercalate "." ms ++ "." ++ n
    show (LocalIdentifier n) = n