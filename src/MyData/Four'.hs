module MyData.Four' where

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a (f b) (f c) (f d)

instance Monoid a => Applicative (Four' a) where
    pure x = Four' mempty x x x
    (Four' x f g g') <*> (Four' a b c d) = Four' (x <> a) (f b) (g c) (g' d)
