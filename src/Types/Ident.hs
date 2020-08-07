module Types.Ident where
import Data.List (intercalate)
import Control.Monad (replicateM)

type Name = String

names :: [Name]
names = [1..] >>= flip replicateM ['a'..'z']

type Module = [Name]

data Identifier
    = ExternalIdentifier Module Name
    | LocalIdentifier Name
    deriving(Eq, Ord)

instance Show Identifier where
    show (ExternalIdentifier ms n) = intercalate "." ms ++ "." ++ n
    show (LocalIdentifier n) = n