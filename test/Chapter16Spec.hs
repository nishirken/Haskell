module Chapter16Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter16

functorCompose' :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose' x f g = (fmap (g . f) x) == (fmap g . fmap f $ x)

functorId' :: (Eq (f a), Functor f) => f a -> Bool
functorId' x = (fmap id x) == (id x)

chapter16spec :: SpecWith ()
chapter16spec =
    describe "Chapter16 Functor" $ do
        context "Functor instances" $ do
            it "Identity id" $ property $ do
                functorId' (Identity 2)

            it "Identity compose" $ property $ do
                functorCompose' (Identity 2) (+3) (*3)

            it "Pair id" $ property $ do
                functorId' (Pair 1 3)

            it "Pair compose" $ property $ do
                functorCompose' (Pair 1 3) (+1) (*3)

            it "Two id" $ do
                fmap id (Two 'a' 1) `shouldBe` id (Two 'a' 1)

            it "Two compose" $ do
                fmap ((+1) . (*3)) (Two 'a' 1) `shouldBe` (Two 'a' 4)

            it "Three id" $ do
                fmap id (Three "1" 'a' (3 :: Int)) `shouldBe` id (Three "1" 'a' (3 :: Int))

            it "Three compose" $ do
                fmap ((+1) . (*3)) (Two 'a' 1) `shouldBe` (Two 'a' 4)

            it "Three' id" $ do
                fmap id (Three' "1" (2 :: Int) (3 :: Int)) `shouldBe` id (Three' "1" (2 :: Int) (3 :: Int))

            it "Three' compose" $ do
                fmap ((+1) . (*3)) (Three' "1" 2 3) `shouldBe` (Three' "1" 7 10)

            it "Four' id" $ do
                functorId' (Four' 1 3 7 'a')

            it "Four' compose" $ do
                functorCompose' (Four' 1 3 2 4) (*2) (+3)

