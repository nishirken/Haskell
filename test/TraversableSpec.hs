module TraversableSpec where

import Data.Functor.Compose (Compose (..))
import Test.Hspec (context, describe, it, SpecWith)
import Test.QuickCheck (Arbitrary, arbitrary, property, elements)
import Test.QuickCheck.Classes (traversable)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import CommonArbitrary
import MyData.Identity
import MyData.Constant
import MyData.Optional
import MyData.List
import MyData.Three
import MyData.Three'
import MyData.S
import MyData.Tree

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        elements [None, Some x]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

traversableSpec :: SpecWith ()
traversableSpec = do
    describe "Chapter-21 Traversable" $ do
        it "Identity" $ quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
        it "Constant" $ quickBatch $ traversable (undefined :: Constant (Int, Int, [Int]) (Char, Char, [Char]))
        it "Optional" $ quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
        it "List" $ quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
        it "Three" $ quickBatch $ traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Char, Char, [Char]))
        it "Three'" $ quickBatch $ traversable (undefined :: Three' (Int, Int, [Int]) (Char, Char, [Char]))
        it "S" $ quickBatch $ traversable (undefined :: S [] (Int, Int, [Int]))
        it "Tree" $ quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
