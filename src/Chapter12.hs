module Chapter12 where

import Data.List.Utils

notThe :: String -> Maybe String
notThe x =
    if x == "the"
        then Nothing
        else Just x

replaceThe :: String -> String
replaceThe = replace "the" "a"
