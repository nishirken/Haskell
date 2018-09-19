module CommonArbitrary where

import Control.Monad (replicateM)
import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary, Gen, sized, variant)

import MyData.Identity
import MyData.Pair
import MyData.Two
import MyData.Three
import MyData.Three'
import MyData.Four
import MyData.Four'
import MyData.List

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance Arbitrary a => Arbitrary (Pair a)  where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Pair x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Three' x y y

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        z' <- arbitrary
        return (Four x y z z')

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
