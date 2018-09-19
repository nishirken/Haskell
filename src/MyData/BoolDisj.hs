module MyData.BoolDisj where

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False
