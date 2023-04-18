module Name where

import Data.Text (Text, unpack, intercalate)

type Namespace = [Text]
type Identifier = Text
data Name
    = Name Namespace Identifier
    deriving (Eq, Ord)

showNamespace :: Namespace -> String
showNamespace parts = unpack (intercalate "." parts)

isQualified :: Name -> Bool
isQualified (Name n _) = not (null n)

unqualified :: Text -> Name
unqualified = Name []

getIdentifier :: Name -> Text
getIdentifier (Name _ i) = i

getNamespace :: Name -> Namespace
getNamespace (Name n _) = n

instance Show Name where
    show (Name n i) = unpack (intercalate "." (n ++ [i]))
