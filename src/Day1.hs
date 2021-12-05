module Day1 (example, part1, part2) where

example :: [String]
example = 
    [ "199"
    , "200"
    , "208"
    , "210"
    , "200"
    , "207"
    , "240"
    , "269"
    , "260"
    , "263"
    ]

parse :: String -> Int
parse = read

lesserPairs :: [Int] -> [(Int, Int)]
lesserPairs values = [(l, r) | (l, r) <- zip values (drop 1 values), l < r]

part1 :: [String] -> String
part1 input =
    let values = fmap parse input
        result = length $ lesserPairs values
     in show result

part2 :: [String] -> String
part2 input =
    let values = fmap parse input
        tripletSums = [ v1 + v2 + v3 | (v1, v2, v3) <- zip3 values (drop 1 values) (drop 2 values) ]
     in show $ length $ lesserPairs tripletSums