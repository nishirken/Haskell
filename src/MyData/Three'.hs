module MyData.Three' where

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' x f g) <*> (Three' a b c) = Three' (x <> a) (f b) (g c)

instance Foldable (Three' a) where
    foldr f initial (Three' x y z) = f y (f z initial)
    foldl f initial (Three' x y z) = f (f initial y) z
