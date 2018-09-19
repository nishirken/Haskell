module MyData.OneOrOther where

data OneOrOther a b =
    One a
    | Other b
    deriving (Eq, Show)

instance Functor (OneOrOther a) where
    fmap f (Other x) = Other (f x)
    fmap _ (One x) = One x
