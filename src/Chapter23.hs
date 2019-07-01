module Chapter23 where

import Text.Show.Functions -- show for ->

newtype State s a = State { runState :: s -> (a, s) } deriving (Show)

instance Functor (State s) where
  fmap f (State g) = State $ \s -> ((f . fst . g) s, s)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State sab) <*> (State sa) = State $ \s -> ((fst $ sab s) (fst $ sa s), s)
