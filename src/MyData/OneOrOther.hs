module MyData.OneOrOther where

data OneOrOther a b =
    One a
    | Other b
    deriving (Eq, Show)

instance Functor (OneOrOther a) where
    fmap f (Other x) = Other (f x)
    fmap _ (One x) = One x

instance Applicative (OneOrOther a) where
    pure = Other
    _ <*> (One x) = One x
    (One f) <*> _ = One f
    (Other f) <*> (Other x) = Other (f x)


instance Monad (OneOrOther a) where
    return = pure
    (One x) >>= _ = One x
    (Other x) >>= f = f x
    