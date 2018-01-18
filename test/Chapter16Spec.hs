module Chapter16Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter16 (Pair (Pair))

functorCompose' :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose' x f g = (fmap (g . f) x) == (fmap g . fmap f $ x)

chapter16spec :: SpecWith ()
chapter16spec =
    describe "Chapter16 Functor" $ do
        context "Functor instances" $ do
            it "Pair works" $ do
                fmap ((+1) . (*3)) (Pair 1 3) `shouldBe` (Pair 4 10)

