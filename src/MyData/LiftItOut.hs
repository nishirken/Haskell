module MyData.LiftItOut where

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut g) = LiftItOut (fmap f g)
