module Chapter15 (
	Optional (Nada, Only)
	, Trivial (Trivial)
	, Identity (Identity)
	, Two (Two)
	, Three (Three)
	, Four (Four)
	, BoolConj (BoolConj)
	, BoolDisj (BoolDisj)
	, Or (Fst, Snd)
	, Validation (Success, Failure)
	) where

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

instance Semigroup Trivial where
	_ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
	Identity b <> Identity c = Identity (b <> c)

data Two a = Two a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Two a) where
	Two a b <> Two c d = Two (a <> c) (b <> d)

data Three a = Three a a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Three a) where
	Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

data Four a = Four a a a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Four a) where
	Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
	BoolConj a <> BoolConj b = BoolConj (a && b)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
	BoolDisj a <> BoolDisj b = BoolDisj (a || b)

data Or a b =
	Fst a
	| Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
	Fst a <> Fst b = Fst b
	Fst _ <> Snd a = Snd a
	Snd a <> Fst _ = Snd a
	Snd a <> Snd b = Snd a

newtype Combine a b =
	Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
	Combine f <> Combine g = Combine $ \x -> f x <> g x

newtype Comp a =
	Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
	Comp f <> Comp g = Comp $ f . g

data Validation a b =
	Failure a | Success b
	deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
	Failure a <> Success _ = Failure a
	Success _ <> Failure a = Failure a
	Success a <> Success b = Success (a <> b)
	Failure a <> Failure b = Failure (a <> b)
