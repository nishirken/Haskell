module Chapter17 (
	Identity (Identity)
	, Pair (Pair)
	) where

import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: (Maybe Integer, Maybe Integer)
tupled = (,) y z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

newtype Identity a = Identity a
	deriving (Eq, Ord, Show)

instance Functor Identity where
	fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
	pure = Identity
	(Identity f) <*> (Identity x) = Identity (f x)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
	fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
	pure x = Pair x x
	(Pair f g) <*> (Pair x y) = Pair (f x) (g y)
