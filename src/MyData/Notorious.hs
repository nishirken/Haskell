module MyData.Notorious where

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious g g' g'') = Notorious g g' (fmap f g'')
