module MonadSpec (monadSpec) where

import Test.QuickCheck (Arbitrary, arbitrary, property)
import Test.Hspec (context, describe, it, SpecWith, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSize)
import CommonArbitrary

import MyData.Nope
import MyData.Identity
import MyData.OneOrOther
import MyData.List
import MonadUtils (j, l1, l2, a, meh, flipType)

leftIdentity :: (Eq (m b), Monad m) => a -> (a -> m b) -> Bool
leftIdentity value f = (return value >>= f) == f value

testLeftIdentity f = it "Left identity" $ property f

rightIdentity :: (Eq (m a), Monad m) => m a -> Bool
rightIdentity monad = (monad >>= return) == monad

testRightIdentity f = it "Right identity" $ property f

assoc :: (Eq (m a), Eq (m b), Eq (m c), Monad m) => m a -> (a -> m b) -> (b -> m c) -> Bool
assoc monad1 f g = ((monad1 >>= f) >>= g) == (monad1 >>= (\x -> f x >>= g))

testAssoc f = it "Associativity" $ property f

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

monadSpec :: SpecWith ()
monadSpec =
  describe "Chapter18 Monad" $ do
    context "Nope" $ do
      testLeftIdentity (\x -> leftIdentity (x :: Int) (const NopeDotJpg))
      testRightIdentity (\x -> rightIdentity (x :: Nope Int))
      testAssoc (\x -> assoc (x :: Nope Int) (const NopeDotJpg) (const NopeDotJpg))

    context "Identity" $ do
      testLeftIdentity (\x -> leftIdentity (x :: Int) Identity)
      testRightIdentity (\x -> rightIdentity (x :: Identity Int))
      testAssoc (\x -> assoc (x :: Identity Int) Identity Identity)

    context "OneOrOther" $ do
      testLeftIdentity (\x y -> leftIdentity (x :: Int) (\_ -> y :: OneOrOther String Int))
      testRightIdentity (\x -> rightIdentity (x :: OneOrOther String Int))
      testAssoc (\x y z -> assoc (x :: OneOrOther String Int) (\_ -> y :: OneOrOther String Int) (\_ -> z :: OneOrOther String Int))

    context "List" $ do
      testLeftIdentity (\x y -> leftIdentity (x :: Int) (\_ -> y :: List Int))
      testRightIdentity (\x -> rightIdentity (x :: List Int))
      modifyMaxSize (const 20) $
        testAssoc (\x y z -> assoc (x :: List Int) (\_ -> y :: List Int) (\_ -> z :: List Int))

    context "j util" $ do
      it "Works with List" $ j [[1, 2], [3]] `shouldBe` [1, 2, 3]
      it "Works with Just" $ j (Just (Just 1)) `shouldBe` Just 1

    context "l1 util" $ do
      it "Works with List" $ l1 (+ 1) [1, 2] `shouldBe` [2, 3]
      it "Works with Just" $ l1 (* 3) (Just 4) `shouldBe` Just 12

    context "l2 util" $ do
      it "Works with List" $ l2 (+) [1, 2] [3, 4, 6] `shouldBe` [4, 5, 7, 5, 6, 8]
      it "Works with Just" $ l2 (*) (Just 4) (Just 2) `shouldBe` Just 8

    context "a util" $ do
      it "Works with List" $ a [1, 2] [(+ 3), (+ 4), (* 6)] `shouldBe` [4, 5, 5, 6, 6, 12]
      it "Works with Just" $ a (Just 4) (Just (* 5)) `shouldBe` Just 20

    context "meh util" $ do
      it "Works with List" $ meh [1, 2] (\x -> [x, x]) `shouldBe` [[1, 2], [1, 2], [1, 2], [1, 2]]
      it "Works with Just" $ meh [4, 2] (\x -> Just x) `shouldBe` Just [4, 2]

    context "flipType util" $ do
      it "Works with List" $ flipType [[1, 2], [3, 4]] `shouldBe` [[1, 3], [1, 4], [2, 3], [2, 4]]
      it "Works with Just" $ flipType [Just 4, Just 2, Just 3] `shouldBe` Just [4, 2, 3]
      it "Works with Nothing" $ flipType [Just 4, Nothing, Just 3] `shouldBe` Nothing