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

data K a b = K a

instance Functor (K a) where
    fmap f (K x) = K x

newtype T a b = T a

instance Functor (T a) where
    fmap f (T x) = T x
