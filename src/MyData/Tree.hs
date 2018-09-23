module MyData.Tree where

data Tree a =
    Empty
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Node left x right) = (foldMap f left) <> (f x) <> (foldMap f right)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right
