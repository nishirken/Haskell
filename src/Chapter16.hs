{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

-- Lifting Exercises

a = (fmap (+ 1)  $ read "[1]" :: [Int]) !! 0 -- 2

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"]) -- Just ["Hi,lol","Hellolol"]

c = fmap (* 2) (\x -> x - 2) -- c 1 -> -2

d = fmap ((++) "1" . show) (\x -> [x, 1..3]) -- d 0 -> "1[0,1,2,3]"

e :: IO Integer -- 3693
e = fmap (* 3) changed where
    ioi = readIO "1" :: IO Integer
    changed = fmap (read . ("123" ++) . show) ioi

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers x) = Yeppers $ f x
    fmap _ LolNope = LolNope

data OneOrOther a b =
    One a
    | Other b
    deriving (Eq, Show)

instance Functor (OneOrOther a) where
    fmap f (Other x) = Other (f x)
    fmap _ (One x) = One x

data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K x) = K x

newtype T a b = T a deriving (Eq ,Show)

instance Functor (T a) where
    fmap f (T x) = T x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip T a) where
    fmap f (Flip (T a))= Flip (T (f a))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut g) = LiftItOut (fmap f g)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa g g') = DaWrappa (fmap f g) (fmap f g')

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething f' g) = IgnoringSomething f' (fmap f g)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious g g' g'') = Notorious g g' (fmap f g'')

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons a b) = Cons (f a) (fmap f b)
    fmap _ Nil = Nil

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read g) = Read $ f . g
