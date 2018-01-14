module Chapter15Spec (chapter15spec) where

import Test.Hspec
import Data.Monoid
import Chapter15

chapter15spec :: SpecWith ()
chapter15spec =
    describe "Chapter15Spec" $ do
        context "Maybe instance of Monoid" $ do
            it "works with two Only" $ do
                (Only (Sum 1) `mappend` Only (Sum 1)) `shouldBe` (Only $ Sum 2)

            it "works with Only and Nada" $ do
                (Only (Sum 1) `mappend` Nada) `shouldBe` (Only $ Sum 1)

            it "works with Only and Nada" $ do
                (Nada `mappend` Only (Sum 1)) `shouldBe` (Only $ Sum 1)
