module Chapter15Spec (chapter15spec, semigroupSpec) where

import Data.Semigroup
import Test.Hspec
import Chapter15 (Optional (Only, Nada), Trivial (Trivial), Identity (Identity))
import Test.QuickCheck

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
	arbitrary = return Identity Int

type SemigroupAssoc x = x -> x -> x -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupSpec :: SpecWith ()
semigroupSpec =
	describe "Semigroup" $ do
		context "Trivial" $
			it "works" $ property
				(semigroupAssoc :: SemigroupAssoc Trivial)

		context "Trivial" $
        			it "works" $ property
        				(semigroupAssoc :: SemigroupAssoc Identity Int)
