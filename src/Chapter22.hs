module Chapter22 where

import Data.Char (toUpper)
import Control.Applicative (liftA2)
import Text.Show.Functions -- show for ->

newtype Reader r a = Reader { runReader :: r -> a } deriving (Show)

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = liftA2 (,) rev cap

tupled'' :: String -> (String, String)
tupled'' = cap >>= \x -> rev >>= \y -> pure (x, y)

tupled''' :: String -> (String, String)
tupled''' = do
  x <- cap
  y <- rev
  pure (x, y)

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f f' g = f <$> f' <*> g

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  f `fmap` (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \r -> a
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  (Reader ra) >>= f = Reader $ \r -> (runReader $ f (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName   = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person =
  Person
    { humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog =
  Dog
    { dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader $ \person -> dogName person
  addy <- Reader $ \person -> address person
  return $ Dog name addy
