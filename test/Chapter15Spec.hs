{-# LANGUAGE FlexibleContexts #-}

module Chapter15Spec (chapter15Spec) where

import Data.Semigroup
import Test.Hspec
import Chapter15 (
    Optional (Some, None)
    , First' (First')
    , Trivial (Trivial)
    , Identity (Identity)
    , Two (Two)
    , Three (Three)
    , Four (Four)
    , BoolConj (BoolConj)
    , BoolDisj (BoolDisj)
    , Or (Fst, Snd)
    , Combine (Combine)
    , Comp (Comp)
    , Validation (Success, Failure)
    )
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)
import Control.Monad

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

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance Arbitrary a => Arbitrary (Two a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance Arbitrary a => Arbitrary (Three a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance Arbitrary a => Arbitrary (Four a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        z' <- arbitrary
        return (Four x y z z')

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return (BoolConj x)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Fst x, Snd y]

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

chapter15Spec =
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
            testLeftIdentity (leftIdentity :: MonoidIdentity (Two (Sum Int)))
            testRightIdentity (rightIdentity :: MonoidIdentity (Two (Sum Int)))
            testAssoc (assoc :: Assoc (Two (Sum Int)))
    
        context "Three" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Three String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Three String))
            testAssoc (assoc :: Assoc (Three String))

        context "Four" $ do
            testLeftIdentity (leftIdentity :: MonoidIdentity (Four String))
            testRightIdentity (rightIdentity :: MonoidIdentity (Four String))
            testAssoc (assoc :: Assoc (Four String))

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
