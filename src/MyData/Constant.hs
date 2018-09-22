module MyData.Constant where

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr _ initial _ = initial
    foldl _ initial _ = initial
    foldMap _ _ = mempty
