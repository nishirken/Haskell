module Chapter17Spec (chapter17Spec) where

import Test.Hspec
import Test.QuickCheck
import Chapter17 (
    Identity (Identity)
    , Pair (Pair)
    )

applicativeIdentity :: (Eq (f a), Applicative f) => f a -> Bool
applicativeIdentity x = (pure id <*> x) == x

applicativeInterchange :: (Eq (f a), Applicative f) => f (a -> a) -> a -> Bool
applicativeInterchange f x = (f <*> pure x) == (pure ($ x) <*> f)

applicativeComposition :: (Eq (f a), Applicative f) => f (a -> a) -> f (a -> a) -> f a -> Bool
applicativeComposition f g x = (pure (.) <*> f <*> g <*> x) == (f <*> (g <*> x))

chapter17Spec :: SpecWith ()
chapter17Spec =
    describe "Chapter17 Applicative" $ do
		context "Identity" $ do
			it "Id" $ property $ \x -> applicativeIdentity (Identity (x :: Int))
			it "Homomorphism" $ property $
				\x -> (pure (+ x) <*> (pure x :: Identity Int)) == (pure $ (+ x) x)
			it "Interchange" $ property $
				\x -> applicativeInterchange (Identity (+ x)) (x :: Int)
			it "Compose" $ property $
				\x -> applicativeComposition (Identity (+ x)) (Identity (* x)) (Identity (x :: Int))

		context "Pair" $ do
			it "Id" $ property $ \x -> applicativeIdentity (Pair (x :: Int) x)
			it "Homomorphism" $ property $
				\x -> (pure (+ x) <*> (pure x :: Pair Int)) == (pure $ (+ x) x)
			it "Interchange" $ property $
				\x -> applicativeInterchange (Pair (+ x) (* x)) (x :: Int)
			it "Compose" $ property $
				\x -> applicativeComposition (Pair (+ x) (* x)) (Pair (+ 1) (* 3)) (Pair (x :: Int) x)

		context "Two" $ do
			it "Id" $ property $ \x -> applicativeIdentity (Pair (x :: Int) x)
			it "Homomorphism" $ property $
				\x -> (pure (+ x) <*> (pure x :: Pair Int)) == (pure $ (+ x) x)
			it "Interchange" $ property $
				\x -> applicativeInterchange (Pair (+ x) (* x)) (x :: Int)
			it "Compose" $ property $
				\x -> applicativeComposition (Pair (+ x) (* x)) (Pair (+ 1) (* 3)) (Pair (x :: Int) x)
