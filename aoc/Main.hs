module Main where

import Data.Functor ((<&>))
import Data.Foldable (traverse_)

import qualified Paths_aoc as Paths
import Challenge

main :: IO ()
main = do
    traverse_ (\(challenge, num) -> do
        fileLines <- Paths.getDataFileName ("data/" ++ _dataFile challenge) >>= readFile <&> lines 
        putStrLn $ show num ++ ".1: " ++ (_part1 challenge $ fileLines)
        putStrLn $ show num ++ ".2: " ++ (_part2 challenge $ fileLines)
        ) (zip challenges [1..])
