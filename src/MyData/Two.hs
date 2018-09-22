module MyData.Two where

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two c d = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (Two x g) <*> (Two a b) = Two (x <> a) (g b)

instance Foldable (Two a) where
    foldr f initial (Two a b) = f b initial
    foldl f initial (Two a b) = f initial b
