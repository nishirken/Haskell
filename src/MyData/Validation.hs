module MyData.Validation where

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
    Failure a <> Success _ = Failure a
    Success _ <> Failure a = Failure a
    Success a <> Success b = Success (a <> b)
    Failure a <> Failure b = Failure (a <> b)
