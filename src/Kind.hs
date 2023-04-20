module Kind where

data Kind
    = KStar
    | KArrow Kind Kind
    deriving (Show, Eq, Ord)

class HasKind t where
    kind :: t -> Kind
