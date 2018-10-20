module MyData.Optional where

data Optional a =
    None
    | Some a deriving (Eq, Show)

instance Functor Optional where
    fmap f None = None
    fmap f (Some x) = Some $ f x

instance Applicative Optional where
    pure x = Some x
    None <*> _ = None
    _ <*> None = None
    (Some f) <*> (Some x) = Some $ f x

instance Semigroup a => Semigroup (Optional a) where
    a <> None = a
    None <> a = a
    Some a <> Some b = Some (a <> b)

instance Monoid a => Monoid (Optional a) where
    mempty = None

instance Foldable Optional where
    foldr _ initial None = initial
    foldr f initial (Some x) = f x initial

instance Traversable Optional where
    traverse f (Some x) = Some <$> f x
    traverse _ None = pure None
