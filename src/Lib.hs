module Lib (someFunc) where

import qualified Chapter11.HuttonsRazor as HR

someFunc :: IO ()
someFunc = do 
    putStrLn "someFunc"
