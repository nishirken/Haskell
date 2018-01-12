getTime :: IO ()
getTime = do
    line <- getLine
    putStrLn $ line

prepairInputValue :: String -> String
prepairInputValue value = map read $ filter (/= ':') value

matchHours :: Int -> Int
matchHours hours = hours
