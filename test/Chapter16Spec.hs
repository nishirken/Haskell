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

            it "Id works with Nothing" $ property $ functorId' (LolNope :: Possibly Int)

            it "Compose works with Just" $ property $
                \x y -> functorCompose' (Yeppers (x :: Int)) (+ (y :: Int)) (* y)

            it "Compose works with Nothing" $ property $
                \x -> functorCompose' (LolNope :: Possibly Int) (+ (x :: Int)) (* x)

        context "OneOrOther - Either" $ do
            it "Id works with One" $ property $
                \x -> functorId' (One x :: OneOrOther Int Int)

            it "Id works with Other" $ property $
                \x -> functorId' (Other x :: OneOrOther Int Int)

            it "Compose works with One" $ property $
                \x y -> functorCompose' (One x :: OneOrOther Int Int) (+ (y :: Int)) (* y)

            it "Compose works with Other" $ property $
                \x y -> functorCompose' (Other x :: OneOrOther Int Int) (+ (y :: Int)) (* y)

        context "Quant" $ do
            it "Id works with Finance" $ property $ functorId' (Finance :: Quant Char Int)

            it "Id works with Desk" $ property $ \x -> functorId' (Desk x :: Quant Char Int)

            it "Id works with Bloor" $ property $ \x -> functorId' (Bloor x :: Quant Char Int)

            it "Compose works with Finance" $ property $
                \x -> functorCompose' (Finance :: Quant Char String) (++ (x :: String)) (++ x)

            it "Compose works with Desk" $ property $
                \x y -> functorCompose' (Desk x :: Quant Char String) (++ (y :: String)) (++ y)

            it "Compose works with Bloor" $ property $
                \x -> functorCompose' (Bloor x :: Quant Char String) (++ (x :: String)) (++ x)

        context "K" $ do
            it "Id works" $ property $ \x -> functorId' (K (x :: Int))

            it "Compose works" $ property $ \x -> functorCompose' (K (x :: Int)) (+ x) (* x)

        context "T" $ do
            it "Id works" $ property $ \x -> functorId' (T (x :: Int))

            it "Compose works" $ property $ \x -> functorCompose' (T (x :: Int)) (+ x) (* x)

        context "Flip" $ do
            it "Id works" $ property $ \x -> functorId' (Flip (T (x :: Int)))

            it "Compose works" $ property $ \x -> functorCompose' (Flip (T (x :: Int))) (+ x) (* x)

        context "EvilGoateeConst" $ do
            it "Id works" $ property $ \x -> functorId' (GoatyConst (x :: Int))
    
            it "Compose works" $ property $ \x -> functorCompose' (GoatyConst (x :: Int)) (+ x) (* x)

        context "LiftItOut" $ do
            it "Id works" $ functorId' (LiftItOut (Just 2))

            it "Compose works" $ functorCompose' (LiftItOut (Just 3)) (+ 2) (* 4)

        context "Parappa" $ do
            it "Id works" $ functorId' (DaWrappa (Just 2) (Just 3))

            it "Compose works" $ functorCompose' (DaWrappa (Just 3) (Just 4)) (+ 2) (* 4)

        context "IgnoreOne" $ do
            it "Id works" $
                functorId' (IgnoringSomething (Just 2) (Left "2") :: IgnoreOne Maybe (Either String) Int String)

            it "Compose works" $
                functorCompose' (IgnoringSomething (Just 3) (Left "2")) (+ 2) (* 4)

        context "Notorious" $ do
            it "Id works" $ functorId' (Notorious (Just 2) (Just "2") (Just 'a'))

            it "Compose works" $ functorCompose' (Notorious (Just 3) (Just "2") (Just 0.5)) (+ 2) (* 4)

        context "List" $ do
            it "Id works with Cons" $ functorId' $ Cons 0 $ Cons 1 $ Cons 4 Nil
            it "Id works with Nil" $ functorId' (Nil :: List Int)

            it "Compose works with Cons" $ functorCompose' (Cons 0 $ Cons 1 $ Cons 4 Nil) (+ 2) (* 4)
            it "Compose works with Nil" $ functorCompose' Nil (+ 2) (* 4)

        context "GoatLord" $ do
            it "Id works with NoGoat" $ functorId' (NoGoat :: GoatLord Int)
            it "Id works with OneGoat" $ functorId' (OneGoat 1)
            it "Id works MoreGoats"
                $ functorId' (MoreGoats (MoreGoats (OneGoat 2) NoGoat (OneGoat 2)) (OneGoat 2) NoGoat)
    
            it "Compose works with NoGoat" $ functorCompose' NoGoat (+ 2) (* 4)
            it "Compose works with OneGoat" $ functorCompose' (OneGoat 3) (+ 2) (* 4)
            it "Compose works with MoreGoats"
                $ functorCompose' (MoreGoats (MoreGoats (OneGoat 2) NoGoat (OneGoat 2)) (OneGoat 2) NoGoat) (+ 2) (* 4)
