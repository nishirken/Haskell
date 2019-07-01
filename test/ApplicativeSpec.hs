module ApplicativeSpec (applicativeSpec) where

import Data.Monoid (Sum, Product)
import Test.Hspec (context, describe, it, SpecWith)
import Test.QuickCheck (property, mapSize)
import CommonArbitrary

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
import Chapter23 (State (..))

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

applicativeSpec :: SpecWith ()
applicativeSpec =
    describe "Applicative" $ do
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

        context "OneOrOther" $ do
            testIdentity (identity :: Id(OneOrOther Int String))
            testComposition $
                \x y z -> composition (One (x :: Int)) (Other (++ (y :: String))) (z :: OneOrOther Int String)
            testHomomorphism $ \x y -> (pure (* x) <*> pure y) == (pure (x * y) :: OneOrOther String Int)
            testInterchange $ \x y -> interchange (One (x :: Int)) (y :: Int)

        context "S" $ do
            testIdentity (\x -> identity (x :: S Maybe Int))
            testComposition (\x -> composition (S (Just (+ x)) (+ x)) (S (Just (* x)) (* x)) (S (Just x) (x :: Int)))
            testHomomorphism $ \x y -> (pure (* (x :: Int)) <*> pure (y :: Int)) == (pure (x * y) :: S Maybe Int)
            testInterchange (\x y -> interchange (S (Just (+ x)) (+ (x :: Int))) (y :: Int))

        context "Reader" $ do
            let
                readerComposition ::
                    String ->
                    Reader String (String -> Int) ->
                    Reader String (Char -> String) ->
                    Reader String Char ->
                    Bool
                readerComposition x f f' f'' =
                    (runReader $ pure (.) <*> f <*> f' <*> f'') x == (runReader $ f <*> (f' <*> f'')) x
            testIdentity (\x y -> runReader (pure id <*> (x :: Reader String Int)) (y :: String) == (runReader x) y)
            testComposition readerComposition
            testHomomorphism $ \x y z f ->
                (runReader $ pure ((f :: Int -> Int -> Int) (x :: Int)) <*> pure (y :: Int)) (z :: String) ==
                    (runReader (pure (f x y) :: Reader String Int)) z
            testInterchange $ \x y f ->
                (runReader $ (f :: Reader String (Int -> Int)) <*> pure (x :: Int)) (y :: String) ==
                    (runReader $ pure ($ x) <*> f) y

        context "State" $ do
            let
                stateComposition ::
                    String ->
                    State String (String -> Int) ->
                    State String (Char -> String) ->
                    State String Char ->
                    Bool
                stateComposition x f f' f'' =
                    (runState $ pure (.) <*> f <*> f' <*> f'') x == (runState $ f <*> (f' <*> f'')) x
            testIdentity (\x y -> runState (pure id <*> (x :: State String Int)) (y :: String) == (runState x) y)
            testComposition stateComposition
            testHomomorphism $ \x y z f ->
                (runState $ pure ((f :: Int -> Int -> Int) (x :: Int)) <*> pure (y :: Int)) (z :: String) ==
                    (runState (pure (f x y) :: State String Int)) z
            testInterchange $ \x y f ->
                (runState $ (f :: State String (Int -> Int)) <*> pure (x :: Int)) (y :: String) ==
                    (runState $ pure ($ x) <*> f) y
