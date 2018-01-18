module Chapter16 (Pair (Pair)) where

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
