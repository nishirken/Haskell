module MyData.Constant where

data Constant a b = Constant a deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ initial _ = initial
    foldl _ initial _ = initial
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure (Constant x)
