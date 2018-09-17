module Chapter17 (
    Identity (Identity)
    , Pair (Pair)
    , Two (Two)
    , List (Cons, Nil)
    , Three (Three)
    , Three' (Three')
    , Four (Four)
    , Four' (Four')
    ) where

import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: (Maybe Integer, Maybe Integer)
tupled = (,) y z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

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

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (Two x g) <*> (Two a b) = Two (x <> a) (g b)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (Three x y f) <*> (Three a b c) = Three (x <> a) (y <> b) (f c)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' x f g) <*> (Three' a b c) = Three' (x <> a) (f b) (g c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four x y z z') = Four x y z (f z')

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four x y z f) <*> (Four a b c d) = Four (x <> a) (y <> b) (z <> c) (f d)

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a (f b) (f c) (f d)

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty x x x
    (Four' x f g g') <*> (Four' a b c d) = Four' (x <> a) (f b) (g c) (g' d)
