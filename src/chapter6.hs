data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

instance Eq DayOfWeek where
    _ == _ = True

x :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show (x 10))

-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving (Eq, Show)
settleDown x =
    if x == Woot then Blah else x

-- 3
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- 4
type Rocks = String
type Yeah = Bool

data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

phew = Papu "chases" True
truth = Papu "chomskydoz" True

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Rocks -> Rocks -> Bool
comparePapus p p' = p > p'

-- 5
i :: Num a => a
i = 1
f :: RealFrac a => a
f = 1
