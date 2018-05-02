module Chapter16Spec where

import Test.Hspec
import Test.QuickCheck
import Chapter16

functorCompose' :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose' x f g = fmap (g . f) x == (fmap g . fmap f $ x)

functorId' :: (Eq (f a), Functor f) => f a -> Bool
functorId' x = (fmap id x) == (id x)

chapter16spec :: SpecWith ()
chapter16spec =
    describe "Chapter16 Functor" $ do
        context "Functor instances" $ do
            it "Identity id" $ property $
                \x -> functorId' (Identity (x :: Int))

            it "Identity compose" $ property $
                \x y -> functorCompose' (Identity (x :: Int)) (+y) (*y)

            it "Pair id" $ property $
                \x y -> functorId' (Pair (x :: Int) (y :: Int))

            it "Pair compose" $ property $
                functorCompose' (Pair 1 3) (+1) (*3)

            it "Two id" $
                fmap id (Two 'a' 1) `shouldBe` id Two 'a' 1

            it "Two compose" $
                fmap ((+1) . (*3)) (Two 'a' 1) `shouldBe` (Two 'a' 4)

            it "Three id" $
                fmap id (Three "1" 'a' (3 :: Int)) `shouldBe` id Three "1" 'a' (3 :: Int)

            it "Three compose" $
                fmap ((+1) . (*3)) (Two 'a' 1) `shouldBe` (Two 'a' 4)

            it "Three' id" $
                fmap id (Three' "1" (2 :: Int) (3 :: Int)) `shouldBe` id (Three' "1" (2 :: Int) (3 :: Int))

            it "Three' compose" $
                fmap ((+1) . (*3)) (Three' "1" 2 3) `shouldBe` (Three' "1" 7 10)

            it "Four' id" $
                functorId' (Four' 1 3 7 'a')

            it "Four' compose" $
                functorCompose' (Four' 1 3 2 4) (*2) (+3)

        context "Functor for Possibly - Maybe" $ do
            it "Id works with Just" $ property $
                \x -> functorId' (Yeppers (x :: Int))

            it "Compose works with Just" $ property $
                \x -> functorCompose' (Yeppers (x :: Int)) (+3) (*3)

        context "OneOrOther - Either" $ do
            it "Compose works with One" $
                fmap ((+1) . (*3)) (One 2) `shouldBe` One 2

            it "Compose works with Other" $
                fmap ((+1) . (*3)) (Other 3 :: OneOrOther Int Int) `shouldBe` Other 10
