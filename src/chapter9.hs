import Data.Time
import Data.Char
import Data.List

myEnumFromTo :: (Enum a, Eq a) => a -> a -> [a]
myEnumFromTo x y = iter [y] y where
    iter acc counter
        | counter == x = acc
        | otherwise = iter ((pred counter):acc) (pred counter)

sentence = "all i wanna do is have some fun"

split :: String -> Char -> [String]
split str pattern = iter str [] where
    iter :: String -> [String] -> [String]
    iter rest acc
        | null rest = acc
        | otherwise = iter nextRest (acc ++ [word]) where
            notPattern = (/= pattern)
            word = takeWhile notPattern rest
            nextRest = (drop 1 . dropWhile notPattern) rest

spacesShould = ["all", "i", "wanna", "do", "is", "have", "some", "fun"]

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str = split str '\n'

linesShould =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?" ]

-- filter
articles = ["the", "a", "an"]
removeArticles :: String -> [String]
removeArticles "" = []
removeArticles str = filter (\word -> not $ isInfixOf (map toLower word) (unwords articles)) $ words str

-- zip
zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys)
    | (null xs || null ys) = [(x, y)]
    | otherwise = (x, y) : (zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' pred (x:xs) (y:ys)
    | (null xs || null ys) = [pred x y]
    | otherwise = (pred x y) : (zipWith' pred xs ys)

zip'' :: [a] -> [b] -> [(a, b)]
zip'' x y = zipWith' (\a b -> (a, b)) x y

-- 9.12
upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs

upperString :: String -> String
upperString "" = ""
upperString (x:xs)
    | null xs = [toUpper x]
    | otherwise = toUpper x : upperString xs

upperHead :: String -> Char
upperHead "" = error "Empty string"
upperHead str = (toUpper . head) str

-- own standart functions
-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x = True
    | otherwise = myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs)
    | pred x = True
    | otherwise = myAny pred xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem list = myAny (== elem) list

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem = myAny (== elem)

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs)
    | null xs = [x]
    | otherwise = myReverse xs ++ [x]

-- 5
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

-- 6
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap _ [] = []
myConcatMap pred (x:xs) = pred x ++ myConcatMap pred xs

myConcat' :: [[a]] -> [a]
myConcat' [] = []
myConcat' list = myConcatMap id list

-- 7, 8
type MaxMin a = [a] -> a
type MaxMinBy a = (a -> a -> Ordering) -> MaxMin a

maxMin :: Ordering -> MaxMinBy a
maxMin _ _ [] = undefined
maxMin _ _ [x] = x
maxMin equality pred (x:y:ys) = maxMin equality pred (greater : ys)
    where greater = if pred x y == equality then x else y

myMaximumBy :: MaxMinBy a
myMaximumBy = maxMin GT

myMinimumBy :: MaxMinBy a
myMinimumBy = maxMin LT

-- 9, 10
myMaximum :: (Ord a) => MaxMin a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => MaxMin a
myMinimum = myMinimumBy compare
        
-- folding
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbString "Hello, world!"
    , DbNumber 31
    , DbNumber 9
    , DbNumber 213
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getTimes []
    where
        getTimes :: DatabaseItem -> [UTCTime] -> [UTCTime]
        getTimes (DbDate x) y = x:y
        getTimes _ y = y

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNums []
    where
        getNums :: DatabaseItem -> [Integer] -> [Integer]
        getNums (DbNumber x) y = x:y
        getNums _ y = y
    
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = minimum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

average :: (Fractional a, Real b) => [b] -> a
average [] = error "Can not calculate average on the empty list"
average list = realToFrac (sum list) / fromIntegral (length list)

avgDb :: [DatabaseItem] -> Double
avgDb = average . filterDbNumber
