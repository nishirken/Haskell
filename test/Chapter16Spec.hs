module Chapter16Spec (chapter16Spec) where

import Test.Hspec
import Test.QuickCheck
import Chapter16

functorCompose' :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose' x f g = fmap (g . f) x == (fmap g . fmap f $ x)

functorId' :: (Eq (f a), Functor f) => f a -> Bool
functorId' x = (fmap id x) == (id x)

chapter16Spec :: SpecWith ()
chapter16Spec =
    describe "Chapter16 Functor" $ do
        context "Lifting Exercises" $ do
            it "a" $ a `shouldBe` 2
            it "b" $ b `shouldBe` Just ["Hi,lol","Hellolol"]
            it "c" $ (c 1) `shouldBe` - 2
            it "d" $ (d 0) `shouldBe` "1[0,1,2,3]"

        context "Functor instances" $ do
            it "Identity id" $ property $
                \x -> functorId' (Identity (x :: Int))

            it "Identity compose" $ property $
                \x -> functorCompose' (Identity (x :: Int)) (+ x) (* x)

            it "Pair id" $ property $
                \x -> functorId' (Pair (x :: Int) x)

            it "Pair compose" $ property $
                \x -> functorCompose' (Pair (x :: Int) x) (+ x) (* x)

            it "Two id" $ property $
                \x y -> functorId' (Two (x :: Char) (y :: Int))

            it "Two compose" $ property $
                \x y -> functorCompose' (Two (x :: Char) (y :: Int)) (+ y) (* y)

            it "Three id" $ property $
                \x y z -> functorId' (Three (x :: String) (y :: Char) (z :: Int))

            it "Three compose" $ property $
                \x y z -> functorCompose' (Three (x :: Char) (y :: Int) (z :: String)) ("22" ++) ('c' :)

            it "Three' id" $ property $
                \x y -> functorId' (Three' (x :: String) (y :: Int) y)

            it "Three' compose" $ property $
                \x y -> functorCompose' (Three (x :: String) (y :: Int) y) (+ y) (* y)

            it "Four id" $ property $
                \x y z x' -> functorId' (Four (x :: Char) (y :: Float) (z :: String) (x' :: Int)) 

            it "Four compose" $ property $
                \x y z x' -> functorCompose' (Four (x :: Char) (y :: Float) (z :: String) (x' :: Int)) (+ x') (* x')

            it "Four' id" $ property $
                \x y -> functorId' (Four' (x :: String) x x (y :: Char))

            it "Four' compose" $ property $
                \x y -> functorCompose' (Four' (x :: Char) x x (y :: String)) (y ++) (x :)

        context "Functor for Possibly - Maybe" $ do
            it "Id works with Just" $ property $
                \x -> functorId' (Yeppers (x :: Int))

            it "Id works with Nothing" $ fmap id LolNope `shouldBe` id LolNope

            it "Compose works with Just" $ property $
                \x y -> functorCompose' (Yeppers (x :: Int)) (+ (y :: Int)) (* y)

            it "Compose works with Nothing" $
                functorCompose' LolNope (+ 1) (* 3)

        context "OneOrOther - Either" $ do
            it "Compose works with One" $ property $
                \x y -> functorId' 

            it "Compose works with Other" $
                fmap ((+1) . (*3)) (Other 3 :: OneOrOther Int Int) `shouldBe` Other 10
