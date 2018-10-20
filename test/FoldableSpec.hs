module FoldableSpec (foldableSpec) where

import Test.Hspec (context, describe, it, SpecWith, shouldBe)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, property, NonEmptyList (..))
import Data.Foldable (toList)
import Data.Monoid (Product (..), Sum (..))

import CommonArbitrary
import FoldableUtils (
    mySum
    , myProduct
    , myElem
    , myNull
    , myLength
    , myToList
    , myFold
    , myFoldMap
    , filterF
    , myMinimum
    , myMaximum
    )
import MyData.Constant
import MyData.Two
import MyData.Three
import MyData.Three'
import MyData.Four'
import qualified Data.List.NonEmpty as ListNonEmpty

foldableSpec :: SpecWith ()
foldableSpec = do
    describe "Chapter20 - Foldable" $ do
        it "sum" $ property $ \x -> mySum (x :: [Int]) == sum x
        it "product" $ property $ \x -> myProduct (x :: [Int]) == product x
        it "elem" $ property $ \x xs -> myElem (x :: Int) (xs :: [Int]) == elem x xs
        it "minimum" $ myMinimum ([1, 3, 4, 5] :: [Int]) == minimum [1, 3, 4, 5]
        it "maximum" $ myMaximum ([1, 3, 4, 5] :: [Int]) == maximum [1, 3, 4, 5]
        it "null" $ property $ \xs -> myNull (xs :: [Int]) == null xs
        it "length" $ property $ \xs -> myLength (xs :: [Int]) == length xs

        context "myToList" $ do
            it "with List" $ property $ \xs -> myToList (xs :: [Int]) == toList xs
            it "with Maybe" $ property $ \xs -> myToList (xs :: (Maybe Int)) == toList xs

        context "foldMap" $ do
            it "with Product" $ (myFoldMap (* 3) [3, 4] :: Product Int) `shouldBe` Product 108
            it "with Sum" $ (myFoldMap (+ 22) [0, -4, 2] :: Sum Int) `shouldBe` Sum 64

        context "fold" $ do
            it "with Product" $ (myFold [3, 4] :: Product Int) `shouldBe` Product 12
            it "with Sum" $ (myFold [0, -4, 2] :: Sum Int) `shouldBe` Sum (-2)

        it "filterF" $ (filterF (< 0) [-1, 0, 4, -2]) `shouldBe` [-1, -2]

        context "Constant" $ do
            it "foldr" $ property $
                \x y -> foldr (+) (y :: Int) ((Constant x) :: Constant Char Int) `shouldBe` y
            it "foldl" $ property $
                \x y -> foldl (++) (y :: String) ((Constant x) :: Constant Int String) `shouldBe` y
            it "foldMap" $ property $
                \x y -> (foldMap (+ (x :: Sum Int)) ((Constant y) :: Constant Char (Sum Int)) :: Sum Int) `shouldBe` Sum 0

        context "Two" $ do
            it "foldr" $ property $ \x y -> foldr (+) 0 (Two (x :: String) (y :: Int)) `shouldBe` y
            it "foldl" $ property $ \x y -> foldl (+) 0 (Two (x :: String) (y :: Int)) `shouldBe` y
            it "foldMap" $ property $ \x y -> (foldMap (+ 0) (Two (x :: String) (y :: Sum Int))) `shouldBe` y

        context "Three" $ do
            it "foldr" $ property $ \x y z -> foldr (*) 1 (Three (x :: String) (y :: Char) (z:: Int)) `shouldBe` z
            it "foldl" $ property $ \x y z -> foldl (*) 0 (Three (x :: String) (y :: Char) (z :: Int)) `shouldBe` 0
            it "foldMap" $ property $
                \x y z -> (foldMap (* 1) (Three (x :: String) (y :: Char) (z :: Product Int))) `shouldBe` z

        context "Three'" $ do
            it "foldr" $ (foldr (*) 1 (Three' "" 2 4)) `shouldBe` 8
            it "foldl" $ (foldl (*) 0 (Three' 'a' 2 4)) `shouldBe` 0
            it "foldMap" $ (foldMap (* 2) (Three' [] 4 3) :: Product Int) `shouldBe` Product 48

        context "Four'" $ do
            it "foldr" $ (foldr (*) 1 (Four' "" 2 4 4)) `shouldBe` 32
            it "foldl" $ (foldl (+) 10 (Four' 'a' 2 4 (-1))) `shouldBe` 15
            it "foldMap" $ (foldMap (+ 2) (Four' [] 4 3 (- 4)) :: Sum Int) `shouldBe` Sum 9
