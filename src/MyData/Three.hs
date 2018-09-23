module MyData.Three where

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (Three x y f) <*> (Three a b c) = Three (x <> a) (y <> b) (f c)

instance Foldable (Three a b) where
    foldr f initial (Three a b c) = f c initial
    foldl f initial (Three a b c) = f initial c
