module Chapter15 (Optional (Nada, Only), Trivial (Trivial), Identity (Identity)) where

import Data.Semigroup

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend x Nada = x
    mappend Nada x = x
    mappend (Only x) (Only y) = Only $ mappend x y

-- Semigroup
data Trivial = Trivial deriving (Eq, Show)

-- doesn't work _ <> _ = undefined, undefined fail's in tests
instance Semigroup Trivial where
	a <> _ = a

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
	(Identity b) <> (Identity c) = Identity (b <> c)
