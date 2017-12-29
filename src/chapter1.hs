import Data.Number.Nat

-- 1
beside :: Nat -> Nat -> Bool
beside x y = succ x == y || succ y == x

-- 2
beside2 :: Int -> Int -> Int -> Bool
beside2 distance x y = (abs $ x - y) == distance

-- 3 TODO


-- 4
pow :: Nat -> Nat -> Nat
pow 0 _ = 0
pow _ 0 = 1
pow x degree = x * pow x (degree - 1)

-- 5 TODO


-- 6
