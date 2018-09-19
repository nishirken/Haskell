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
