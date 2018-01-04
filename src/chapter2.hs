module Chapter2 where

-- 2.12
calc = x * 3 + y
    where x = 3; y = 1000

calc' = x * 5
    where y = 10; x = 10 * 5 + y

calc'' = z / x + y
    where x = 7; y = (-x); z = y * 10
