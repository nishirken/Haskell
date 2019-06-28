module CommonArbitrary where

import Control.Monad (replicateM)
import Test.QuickCheck (
    Arbitrary
    , CoArbitrary
    , arbitrary
    , coarbitrary
    , Gen
    , elements
    , sized
    , variant
    , oneof
    )
import Test.QuickCheck.Checkers (EqProp, (=-=), eq)
import Test.QuickCheck.Function (Fun (..), Function, apply)
import Control.Monad (liftM, liftM2)

import MyData.Identity
import MyData.Pair
import MyData.Two
import MyData.Three
import MyData.Three'
import MyData.Four
import MyData.Four'
import MyData.List
import MyData.OneOrOther
import MyData.S
import MyData.Tree
import Chapter22 (Reader (..))

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a)  where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Pair x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        Two x <$> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        Three x y <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        arbitrary >>= \x ->
            arbitrary >>= \y -> return $ Three' x y y

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        Four x y z <$> arbitrary

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (OneOrOther a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [One x, Other y]

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree'
        where
            tree' 0 = pure Empty
            tree' n | n > 0 = 
                oneof [
                    do
                        x <- arbitrary
                        subtree <- tree' $ n `div` 2
                        pure $ Node subtree x subtree
                    , pure Empty
                ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

instance (CoArbitrary r, Arbitrary a) => Arbitrary (Reader r a) where
    arbitrary = Reader <$> arbitrary
