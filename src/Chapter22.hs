{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

newtype Reader s a = Reader { runReader :: s -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


instance Monad (Reader e) where
  mr >>= f =
    let
      step1 r = runReader mr r        -- unwrap inner reader
      step2 r = f (step1 r)           -- apply function to that value
      step3 r = runReader (step2 r) r -- unwrap outer reader
      in Reader (\r -> step3 r) -- package up resulting reader value

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person
  (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)
-- with Reader
getDogR :: Person -> Dog
getDogR = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogR' :: Person -> Dog
getDogR' =
  myLiftA2 Dog dogName address

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2
