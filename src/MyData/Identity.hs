module MyData.Identity where

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity b <> Identity c = Identity (b <> c)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)
