-- 7.4
mTh = \x -> \y -> \z -> x * y * z
mTh1 x = \y -> \z -> x * y *z

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5
mflip = \f -> \x -> \y -> f y x

-- 7.5
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, y, z) (x', y', z') = ((x, x'), (z, z'))

-- 7.6
functionC x y = case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- 7.7
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1
oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- 7.12
tensDigit :: Integral a => a -> (a, a)
tensDigit x = divMod x 10

foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
    | z = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = (read . show) a
main = do
    print (roundTrip 4)
    print (id 4)
