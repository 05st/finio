module Namespace where

import Data.Text (Text, unpack, intercalate)

type Namespace = [Text]

showNamespace :: Namespace -> String
showNamespace parts = unpack (intercalate "::" parts)
