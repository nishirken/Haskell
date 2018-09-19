module MyData.TalkToMe where

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read g) = Read $ f . g
