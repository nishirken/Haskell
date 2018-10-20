module FoldableUtils where

import Data.Maybe (fromMaybe)
import Data.Semigroup (Min (..), getMin, Max (..), getMax)

mySum :: (Foldable t, Num a) => t a -> a
mySum = foldr (+) 0

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = foldr (*) 1

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x = any (== x)

myMinimum :: (Bounded a, Ord a, Foldable t) => t a -> a
myMinimum x = getMin $ foldMap Min x

myMaximum :: (Bounded a, Ord a, Foldable t) => t a -> a
myMaximum x = getMax $ foldMap Max x

myNull :: (Foldable t) => t a -> Bool
myNull = foldr (\_ _ -> False) True

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (:) []

myFold :: (Foldable t, Monoid m) => t m -> m
myFold = myFoldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (\x acc -> (f x) <> acc) mempty

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if (f x) then pure x else mempty)
