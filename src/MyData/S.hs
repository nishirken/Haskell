module MyData.S where

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (fmap f x) (f y)

instance Applicative n => Applicative (S n) where
    pure x = S (pure x) x
    (S n f) <*> (S n' x) = S (n <*> n') $ f x

instance Foldable n => Foldable (S n) where
    foldr f initial (S _ x) = f x initial
    foldMap f (S n x) = foldMap f n <> f x

instance Traversable n => Traversable (S n) where
    traverse f (S n x) = S <$> traverse f n <*> f x
