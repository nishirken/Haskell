module MyData.Combine where

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \_ -> mempty
