module Chapter17Spec (chapter17Spec) where

import Data.Monoid (Sum, Product)
import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM)
import Chapter17 (
    Identity (Identity)
    , Pair (Pair)
    , Two (Two)
    , Three (Three)
    , Three' (Three')
    , Four (Four)
    , Four' (Four')
    , List (Cons, Nil)
    )

type Id functor = functor -> Bool
identity :: (Eq (f a), Applicative f) => f a -> Bool
identity functor = (pure id <*> functor) == functor
testIdentity identityFn = it "Id" $ property identityFn

composition :: (Eq (f a), Applicative f) => f (a -> a) -> f (a -> a) -> f a -> Bool
composition functor1 functor2 functor3 =
    (pure (.) <*> functor1 <*> functor2 <*> functor3) == (functor1 <*> (functor2 <*> functor3))
testComposition compositionFn = it "Composition" $ property compositionFn

testHomomorphism homomorphismFn = it "Homomorphism" $ property homomorphismFn

type Interchange functor = Id functor
interchange :: (Eq (f a), Applicative f) => f (a -> a) -> a -> Bool
interchange functor value = (functor <*> pure value) == (pure ($ value) <*> functor)
testInterchange interchangeFn = it "Interchange" $ property interchangeFn

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= \x -> return $ Identity x

instance Arbitrary a => Arbitrary (Pair a)  where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Pair x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Two x y

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y ->
                arbitrary >>= \z -> return $ Three x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Three' x y y

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y ->
                arbitrary >>= \z ->
                    arbitrary >>= \z' -> return $ Four x y z z'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Four' x y y y

instance CoArbitrary a => CoArbitrary (List a) where
    coarbitrary Nil = variant 0
    coarbitrary (Cons a b) = variant 1 . coarbitrary b
  
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized arbList
        where
            arbList :: Arbitrary a => Int -> Gen (List a)
            arbList 0 = arbitrary >>= \x -> return $ Cons x Nil
            arbList n = do
                x <- arbitrary
                rest <- arbList $ n - 1
                return $ Cons x rest

chapter17Spec :: SpecWith ()
chapter17Spec =
    describe "Chapter17 Applicative" $ do
        context "Identity" $ do
            testIdentity (identity :: Id (Identity Int))
            testComposition $
                \x -> composition (Identity (+ x)) (Identity (* x)) (Identity (x :: Int))
            testHomomorphism $ \x y -> (pure (+ x) <*> pure y) == (pure (x + y) :: Identity Int)
            testInterchange $ \x y -> interchange (Identity (+ (x :: Int))) (y :: Int)

        context "Pair" $ do
            testIdentity (identity :: Id (Pair Int))
            testComposition $
                \x y z -> composition (Pair (+ x) (y -)) (Pair (* x) (y -)) (z :: Pair Int)
            testHomomorphism $ \x y -> (pure (+ x) <*> pure y) == (pure (x + y) :: Pair Int)
            testInterchange $ \x y -> interchange (Pair (+ (x :: Int)) (* (y :: Int))) y

        context "Two" $ do
            testIdentity (identity :: Id (Two String Int))
            testComposition $
                \x y z -> composition (Two (x :: String) (+ (y :: Int))) (Two x (* (y :: Int))) (z :: Two String Int)
            testHomomorphism $ \x y -> (pure (+ x) <*> pure y) == (pure (x + y) :: Two String Int)
            testInterchange $ \x y -> interchange (Two (x :: String) (+ (y :: Int))) y

        context "Three" $ do
            testIdentity (identity :: Id (Three String (Sum Int) Char))
            testComposition $
                \x y z z' -> composition
                (Three (x :: String) (y :: (Sum Int)) (+ (z :: Int)))
                (Three x y (* z))
                (z' :: Three String (Sum Int) Int)
            testHomomorphism $ \x y -> (pure (+ x) <*> pure y) == ((pure (x + y)) :: Three String (Sum Int) Int)
            testInterchange $ \x y z -> interchange (Three (x :: String) (y :: (Sum Int)) (+ (z :: Int))) z

        context "Three'" $ do
            testIdentity (identity :: Id (Three' String Char))
            testComposition $
                \x y z -> composition
                (Three' (x :: String) (+ (y :: Int)) (* y))
                (Three' x (* y) (y -))
                (z :: Three' String Int)
            testHomomorphism $ \x y -> (pure (* x) <*> pure y) == (pure (x * y) :: Three' String Int)
            testInterchange $ \x y -> interchange (Three' (x :: String) (+ (y :: Int)) (* y)) y

        context "Four" $ do
            testIdentity (identity :: Id (Four String (Sum Int) (Product Float) Char))
            testComposition $
                \x y z z' z'' -> composition
                (Four (x :: String) (y :: (Sum Int)) (z :: Product Int) (* (z' :: Int)))
                (Four x y z (+ z'))
                (z'' :: Four String (Sum Int) (Product Int) Int)
            testHomomorphism $
                \x y -> (pure (+ x) <*> pure y) == ((pure (x + y)) :: Four String (Sum Int) (Product Float) Int)
            testInterchange $
                \x y z z' -> interchange (Four (x :: String) (y :: (Sum Int)) (z :: Product Int) (+ (z' :: Int))) z'

        context "Four'" $ do
            testIdentity (identity :: Id (Four' String Char))
            testComposition $
                \x y z -> composition
                (Four' (x :: String) (+ (y :: Int)) (* y) (y -))
                (Four' x (* y) (y -) (+ y))
                (z :: Four' String Int)
            testHomomorphism $ \x y -> (pure (* x) <*> pure y) == (pure (x * y) :: Four' String Int)
            testInterchange $ \x y -> interchange (Four' (x :: String) (+ (y :: Int)) (* y) (y -)) y

        context "List" $ do
            testIdentity (identity :: Id (List Int))
            testComposition $
                \x y z -> composition (Cons (+ (x :: Int)) (Cons (* x) Nil)) (Cons (* (y :: Int)) Nil) (z :: List Int)
            testHomomorphism $ \x y -> (pure (* x) <*> pure y) == (pure (x * y) :: List Int)
            testInterchange $ \x y -> interchange (Cons (+ (x :: Int)) Nil) (y :: Int)
