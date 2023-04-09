module Kind where

data Kind
    = KStar
    | KArrow Kind Kind
    deriving (Show, Eq)

class HasKind t where
    kind :: t -> Kind

