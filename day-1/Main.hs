module Main where

import Data.Functor ((<&>))
import Lib
import System.IO

import qualified Paths_aoc as Paths

loadValues :: IO [Int]
loadValues = Paths.getDataFileName "day-1/input.txt" >>= readFile <&> fmap (read :: String -> Int) . lines

lesserPairs :: [Int] -> [(Int, Int)]
lesserPairs values = [(l, r) | (l, r) <- zip values (drop 1 values), l < r]

part1 :: [Int] -> Int
part1 values =
    let result = length $ lesserPairs values
     in result

part2 :: [Int] -> Int
part2 values =
    let tripletSums = [ v1 + v2 + v3 | (v1, v2, v3) <- zip3 values (drop 1 values) (drop 2 values) ]
     in length $ lesserPairs tripletSums

main :: IO (Int, Int)
main = do
    values <- loadValues
    let result1 = part1 values
    let result2 = part2 values
    return (result1, result2)