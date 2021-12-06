module Challenge
    ( Challenge(..)
    , challenges
    ) where

import qualified Day1
import qualified Day2
import qualified Day3

data Challenge = Challenge 
    { _part1 :: [String] -> String
    , _part2 :: [String] -> String
    , _dataFile :: String
    , _example :: [String]
    , _exampleResult1 :: String
    , _exampleResult2 :: String
    }

challenges :: [Challenge]
challenges = 
    [ Challenge Day1.part1 Day1.part2 "day-1.txt" Day1.example "7" "5"
    , Challenge Day2.part1 Day2.part2 "day-2.txt" Day2.example "150" "900"
    , Challenge Day3.part1 Day3.part2 "day-3.txt" Day3.example "198" "230"
    ]