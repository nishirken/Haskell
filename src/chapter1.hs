import Data.Number.Nat

-- 1
beside :: Nat -> Nat -> Bool
beside x y = succ x == y || succ y == x

-- 2
beside2 :: Nat -> Nat -> Bool
beside2 distance x y = 
