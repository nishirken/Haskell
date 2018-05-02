module TimeMask (prepairInputValue) where

import Data.List.Utils (split)

getTime :: IO ()
getTime = do
    line <- getLine
    putStrLn $ line

prepairInputValue :: String -> [Int]
prepairInputValue value = map read $ split ":" value

matchHours :: Int -> Int
matchHours hours = hours
