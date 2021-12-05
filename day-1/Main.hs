module Main where

import Lib
import System.IO

import qualified Paths_aoc as Paths

loadLines = Paths.getDataFileName "day-1/input.txt" >>= readFile >>= pure . lines

lesser :: [String] -> [(Int, Int)]
lesser lines = [(l, r) | (l, r) <- zip values (drop 1 values), l < r]
    where values = fmap (read :: String -> Int) lines

part1 :: IO Int
part1 = do
    fileLines <- loadLines
    let result = length $ lesser fileLines
    return result

main :: IO Int
main = part1
