module MonadUtils where

j :: Monad m => m (m a) -> m a
j outer = outer >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f monad = fmap f monad

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f monad1 monad2 = monad1 >>= \x -> fmap (f x) monad2

a :: Monad m => m a -> m (a -> b) -> m b
a monad1 monad2 = monad2 <*> monad1

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh list f = foldr (\x acc -> (:) <$> x <*> acc) (return []) (f <$> list)

flipType :: Monad m => [m a] -> m [a]
flipType listOfMonad = meh listOfMonad id
