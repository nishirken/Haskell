module MonoidSpec (monoidSpec) where

import Data.Monoid (Sum)
import Test.Hspec (context, describe, it, SpecWith)
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)
import CommonArbitrary

import MyData.Optional
import MyData.First'
import MyData.Trivial
import MyData.Identity
import MyData.Two
import MyData.Three
import MyData.Four
import MyData.BoolConj
import MyData.BoolDisj
import MyData.Validation
import MyData.List

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        elements [Some x, None]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        return (First' x)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return (BoolConj x)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Failure x, Success y]

type MonoidIdentity x = x -> Bool

leftIdentity :: (Eq m, Monoid m) => m -> Bool
leftIdentity x = mempty `mappend` x == x

rightIdentity :: (Eq m, Monoid m) => m -> Bool
rightIdentity x = x `mappend` mempty == x

testLeftIdentity f = it "Left identity" $ property f
testRightIdentity f = it "Right identity" $ property f

type Assoc x = x -> x -> x -> Bool

assoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
assoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

testAssoc f = it "Assoc" $ property f

monoidSpec :: SpecWith ()
monoidSpec =
    describe "Chapter15 Monoid and Semigroups" $ do
        context "Optional" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Optional String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Optional String))
            testAssoc (assoc :: Assoc (Optional String))

        context "First'" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (First' String))
            testRightIdentity (rightIdentity :: MonoidIdentity (First' String))
            testAssoc (assoc :: Assoc (First' String))

        context "Trivial" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity Trivial)
            testRightIdentity (rightIdentity :: MonoidIdentity Trivial)
            testAssoc (assoc :: Assoc Trivial)

        context "Identity" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Identity String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Identity String))
            testAssoc (assoc :: Assoc (Identity String))

        context "Two" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Two (Sum Int) String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Two (Sum Int) String))
            testAssoc (assoc :: Assoc (Two (Sum Int) String))
    
        context "Three" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Three String (Sum Int) String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Three String (Sum Int) String))
            testAssoc (assoc :: Assoc (Three String (Sum Int) String))

        context "Four" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Four String (Sum Int) String (Sum Int)))
            testRightIdentity (rightIdentity :: MonoidIdentity (Four String (Sum Int) String (Sum Int)))
            testAssoc (assoc :: Assoc (Four String (Sum Int) String (Sum Int)))

        context "BoolConj" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity BoolConj)
            testRightIdentity (rightIdentity :: MonoidIdentity BoolConj)
            testAssoc (assoc :: Assoc BoolConj)

        context "BoolDisj" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity BoolDisj)
            testRightIdentity (rightIdentity :: MonoidIdentity BoolDisj)
            testAssoc (assoc :: Assoc BoolDisj)

        context "Validation" $ do
            testAssoc (assoc :: Assoc (Validation (Sum Int) String))

        context "List" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (List Int))
            testRightIdentity (rightIdentity :: MonoidIdentity (List Int))
            testAssoc (assoc :: Assoc (List Int))
