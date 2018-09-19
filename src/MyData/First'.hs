module MyData.First' where

import MyData.Optional

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
    (First' a) <> (First' b) = First' (a <> b)

instance Monoid a => Monoid (First' a) where
    mempty = First' None
