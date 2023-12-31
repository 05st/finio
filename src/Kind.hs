module Kind where

import Data.Data

data Kind
    = KStar
    | KArrow Kind Kind
    deriving (Show, Eq, Ord, Data)

class HasKind t where
    kind :: t -> Kind
