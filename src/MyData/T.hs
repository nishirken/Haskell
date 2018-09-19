module MyData.T where

newtype T a b = T a deriving (Eq ,Show)

instance Functor (T a) where
    fmap f (T x) = T x
