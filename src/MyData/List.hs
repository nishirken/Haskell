module MyData.List where

data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> x = x
    x <> Nil = x
    (Cons x xs) <> y = Cons x (xs <> y)

instance Monoid (List a) where
    mempty = Nil

instance Applicative List where
    pure x = Cons x Nil
    x <*> Nil = Nil
    Nil <*> x = Nil
    Cons f fs <*> Cons x xs = Cons (f x) ((fs <*> Cons x xs) <> (Cons f fs <*> xs))

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    Cons x xs >>= f = f x <> (xs >>= f)

instance Foldable List where
    foldr f initial Nil = initial
    foldr f initial (Cons x xs) = f x (foldr f initial xs)
    foldl f initial Nil = initial
    foldl f initial (Cons x xs) = foldl f (f initial x) xs
    foldMap f Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
    traverse f = foldr cb $ pure Nil
        where
            cb x xs = Cons <$> f x <*> xs
