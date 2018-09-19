module MyData.Or where

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
