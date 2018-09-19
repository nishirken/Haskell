module MyData.BoolConj where

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True
