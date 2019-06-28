module FunctorSpec (functorSpec) where

import Test.Hspec (context, describe, it, SpecWith)
import Test.QuickCheck (property)
import CommonArbitrary

import MyData.Identity
import MyData.Pair
import MyData.Two
import MyData.Three
import MyData.Three'
import MyData.Four
import MyData.Four'
import MyData.Possibly
import MyData.OneOrOther
import MyData.Quant
import MyData.K
import MyData.T
import MyData.EvilGoateeConst
import MyData.LiftItOut
import MyData.Parappa
import MyData.IgnoreOne
import MyData.Notorious
import MyData.List
import MyData.GoatLord
import MyData.S
import MyData.Tree
import Chapter22 (Reader (..))

type Id x = x -> Bool

identity :: (Eq (f a), Functor f) => f a -> Bool
identity functor = fmap id functor == id functor

testIdentity f = it "Identity" $ property f

composition :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
composition functor f g = fmap (g . f) functor == (fmap g . fmap f $ functor)

testComposition f = it "Composition" $ property f

functorSpec :: SpecWith ()
functorSpec =
    describe "Functor" $ do
        context "Identity" $ do
            testIdentity (identity :: (Id (Identity Int)))
            testComposition $ \x y -> composition (x :: Identity Int) (+ (y :: Int)) (* y)

        context "Pair" $ do
            testIdentity (identity :: (Id (Pair Int)))
            testComposition $ \x y -> composition (x :: Pair Int) (+ (y :: Int)) (* y)

        context "Two" $ do
            testIdentity (identity :: (Id (Two Int String)))
            testComposition $ \x y -> composition (x :: Two Int String) (++ (y :: String)) (y ++)

        context "Three" $ do
            testIdentity (identity :: (Id (Three Int Char Int)))
            testComposition $ \x y -> composition (x :: Three Int Char Int) (+ (y :: Int)) (y -)

        context "Three'" $ do
            testIdentity (identity :: (Id (Three' Char Int)))
            testComposition $ \x y -> composition (x :: Three' Char Int) (+ (y :: Int)) (y -)

        context "Four" $ do
            testIdentity (identity :: (Id (Four Int String Char Int)))
            testComposition $ \x y -> composition (x :: Four Int String Char Int) (+ (y :: Int)) (y -)

        context "Four'" $ do
            testIdentity (identity :: (Id (Four' Char Int)))
            testComposition $ \x y -> composition (x :: Four' Char Int) (+ (y :: Int)) (y -)

        context "Functor for Possibly - Maybe" $ do
            it "Id works with Just" $ property $
                \x -> identity (Yeppers (x :: Int))

            it "Id works with Nothing" $ property $ identity (LolNope :: Possibly Int)

            it "Compose works with Just" $ property $
                \x y -> composition (Yeppers (x :: Int)) (+ (y :: Int)) (* y)

            it "Compose works with Nothing" $ property $
                \x -> composition (LolNope :: Possibly Int) (+ (x :: Int)) (* x)

        context "OneOrOther - Either" $ do
            it "Id works with One" $ property $
                \x -> identity (One x :: OneOrOther Int Int)

            it "Id works with Other" $ property $
                \x -> identity (Other x :: OneOrOther Int Int)

            it "Compose works with One" $ property $
                \x y -> composition (One x :: OneOrOther Int Int) (+ (y :: Int)) (* y)

            it "Compose works with Other" $ property $
                \x y -> composition (Other x :: OneOrOther Int Int) (+ (y :: Int)) (* y)

        context "Quant" $ do
            it "Id works with Finance" $ property $ identity (Finance :: Quant Char Int)

            it "Id works with Desk" $ property $ \x -> identity (Desk x :: Quant Char Int)

            it "Id works with Bloor" $ property $ \x -> identity (Bloor x :: Quant Char Int)

            it "Compose works with Finance" $ property $
                \x -> composition (Finance :: Quant Char String) (++ (x :: String)) (++ x)

            it "Compose works with Desk" $ property $
                \x y -> composition (Desk x :: Quant Char String) (++ (y :: String)) (++ y)

            it "Compose works with Bloor" $ property $
                \x -> composition (Bloor x :: Quant Char String) (++ (x :: String)) (++ x)

        context "K" $ do
            it "Id works" $ property $ \x -> identity (K (x :: Int))

            it "Compose works" $ property $ \x -> composition (K (x :: Int)) (+ x) (* x)

        context "T" $ do
            it "Id works" $ property $ \x -> identity (T (x :: Int))

            it "Compose works" $ property $ \x -> composition (T (x :: Int)) (+ x) (* x)

        context "EvilGoateeConst" $ do
            it "Id works" $ property $ \x -> identity (GoatyConst (x :: Int))
    
            it "Compose works" $ property $ \x -> composition (GoatyConst (x :: Int)) (+ x) (* x)

        context "LiftItOut" $ do
            it "Id works" $ identity (LiftItOut (Just 2))

            it "Compose works" $ composition (LiftItOut (Just 3)) (+ 2) (* 4)

        context "Parappa" $ do
            it "Id works" $ identity (DaWrappa (Just 2) (Just 3))

            it "Compose works" $ composition (DaWrappa (Just 3) (Just 4)) (+ 2) (* 4)

        context "IgnoreOne" $ do
            it "Id works" $
                identity (IgnoringSomething (Just 2) (Left "2") :: IgnoreOne Maybe (Either String) Int String)

            it "Compose works" $
                composition (IgnoringSomething (Just 3) (Left "2")) (+ 2) (* 4)

        context "Notorious" $ do
            it "Id works" $ identity (Notorious (Just 2) (Just "2") (Just 'a'))

            it "Compose works" $ composition (Notorious (Just 3) (Just "2") (Just 0.5)) (+ 2) (* 4)

        context "List" $ do
            it "Id works with Cons" $ identity $ Cons 0 $ Cons 1 $ Cons 4 Nil
            it "Id works with Nil" $ identity (Nil :: List Int)

            it "Compose works with Cons" $ composition (Cons 0 $ Cons 1 $ Cons 4 Nil) (+ 2) (* 4)
            it "Compose works with Nil" $ composition Nil (+ 2) (* 4)

        context "GoatLord" $ do
            it "Id works with NoGoat" $ identity (NoGoat :: GoatLord Int)
            it "Id works with OneGoat" $ identity (OneGoat 1)
            it "Id works MoreGoats"
                $ identity (MoreGoats (MoreGoats (OneGoat 2) NoGoat (OneGoat 2)) (OneGoat 2) NoGoat)
    
            it "Compose works with NoGoat" $ composition NoGoat (+ 2) (* 4)
            it "Compose works with OneGoat" $ composition (OneGoat 3) (+ 2) (* 4)
            it "Compose works with MoreGoats"
                $ composition (MoreGoats (MoreGoats (OneGoat 2) NoGoat (OneGoat 2)) (OneGoat 2) NoGoat) (+ 2) (* 4)

        context "S" $ do
            testIdentity $ (\x -> identity (x :: S Maybe Int))
            testComposition $ \x y -> composition (S (Just x) (x :: Int)) (+ (y :: Int)) (* y)

        context "Tree" $ do
            testIdentity $ (\x -> identity (x :: Tree Int))
            testComposition $ \x y -> composition (x :: Tree Int) (+ (y :: Int)) (* y)

        context "Reader" $ do
            testIdentity $ (\x y ->
                (runReader $ id <$> (x :: Reader String Int)) (y :: String) == (runReader $ id x) y)
            testComposition $ \x y f g ->
                (runReader $
                    fmap ((g :: Char -> Int) . (f :: String -> Char)) (x :: Reader String String)) (y :: String)
                    == (runReader (fmap g . fmap f $ x)) y
