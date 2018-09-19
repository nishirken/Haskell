module MyData.Parappa where

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa g g') = DaWrappa (fmap f g) (fmap f g')
