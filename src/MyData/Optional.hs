module MyData.Optional where

data Optional a =
    None
    | Some a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    a <> None = a
    None <> a = a
    Some a <> Some b = Some (a <> b)

instance Monoid a => Monoid (Optional a) where
    mempty = None
