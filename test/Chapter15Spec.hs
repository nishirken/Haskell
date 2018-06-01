{-# LANGUAGE FlexibleContexts #-}

module Chapter15Spec (chapter15spec, semigroupSpec) where

import Data.Semigroup
import Test.Hspec
import Chapter15 (
	Optional (Only, Nada)
	, Trivial (Trivial)
	, Identity (Identity)
	, Two (Two)
	, Three (Three)
	, Four (Four)
	, BoolConj (BoolConj)
	, BoolDisj (BoolDisj)
	, Or (Fst, Snd)
	)
import Test.QuickCheck
import Control.Monad

chapter15spec :: SpecWith ()
chapter15spec =
    describe "Chapter15Spec" $ do
        context "Maybe instance of Monoid" $ do
            it "works with two Only" $
                (Only (Sum 1) `mappend` Only (Sum 1)) `shouldBe` (Only (Sum 2))

            it "works with Only and Nada" $
                (Only (Sum 1) `mappend` Nada) `shouldBe` (Only $ Sum 1)

            it "works with Only and Nada" $
                (Nada `mappend` Only (Sum 1)) `shouldBe` (Only $ Sum 1)

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

type SemigroupAssoc x = x -> x -> x -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupSpec :: SpecWith ()
semigroupSpec =
	describe "Semigroup" $ do
		it "Trivial" $ property
			(semigroupAssoc :: SemigroupAssoc Trivial)

		it "Identity" $ property
			(semigroupAssoc :: SemigroupAssoc (Identity String))

		it "Two" $ property
			(semigroupAssoc :: SemigroupAssoc (Two String))

		it "Three" $ property
			(semigroupAssoc :: SemigroupAssoc (Three String))

		it "Four" $ property
			(semigroupAssoc :: SemigroupAssoc (Four String))

		it "BoolConj" $ property
			(semigroupAssoc :: SemigroupAssoc BoolConj)

		it "BoolDisj" $ property
			(semigroupAssoc :: SemigroupAssoc BoolDisj)

		it "Or" $ property
			(semigroupAssoc :: SemigroupAssoc (Or String String))
