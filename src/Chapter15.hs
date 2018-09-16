module Chapter15 (
    Optional (Some, None)
    , First' (First')
    , Trivial (Trivial)
    , Identity (Identity)
    , Two (Two)
    , Three (Three)
    , Four (Four)
    , BoolConj (BoolConj)
    , BoolDisj (BoolDisj)
    , Or (Fst, Snd)
    , Combine (Combine)
    , Comp (Comp)
    , Validation (Success, Failure)
    ) where

import Data.Semigroup (Semigroup)

data Optional a =
    None
    | Some a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    a <> None = a
    None <> a = a
    Some a <> Some b = Some (a <> b)

instance Monoid a => Monoid (Optional a) where
    mempty = None

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
    (First' a) <> (First' b) = First' (a <> b)

instance Monoid a => Monoid (First' a) where
    mempty = First' None

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity b <> Identity c = Identity (b <> c)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

data Two a = Two a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Two a) where
    Two a b <> Two c d = Two (a <> c) (b <> d)

instance Monoid a => Monoid (Two a) where
    mempty = Two mempty mempty

data Three a = Three a a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Three a) where
    Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance Monoid a => Monoid (Three a) where
    mempty = Three mempty mempty mempty

data Four a = Four a a a a deriving (Eq, Show)

instance Semigroup a => Semigroup (Four a) where
    Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid a => Monoid (Four a) where
    mempty = Four mempty mempty mempty mempty

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False

data Or a b =
    Fst a
    | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst a <> Fst b = Fst b
    Fst _ <> Snd a = Snd a
    Snd a <> Fst _ = Snd a
    Snd a <> Snd b = Snd a

instance Monoid b => Monoid (Or a b) where
    mempty = Snd mempty

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \_ -> mempty

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ f . g

instance Monoid a => Monoid (Comp a) where
    mempty = Comp $ \_ -> mempty

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
    Failure a <> Success _ = Failure a
    Success _ <> Failure a = Failure a
    Success a <> Success b = Success (a <> b)
    Failure a <> Failure b = Failure (a <> b)
