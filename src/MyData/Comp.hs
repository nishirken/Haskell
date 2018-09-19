module MyData.Comp where

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ f . g

instance Monoid a => Monoid (Comp a) where
    mempty = Comp $ \_ -> mempty
