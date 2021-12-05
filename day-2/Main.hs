module Main where

import Data.Functor ((<&>))
import Lib
import System.IO
import qualified Paths_aoc as Paths

loadValues = Paths.getDataFileName "day-2/input.txt" >>= readFile <&> lines

data Command = Forward Int
             | Up Int
             | Down Int
             deriving (Show)

instance Read Command where
    readsPrec _ input =
        let name = takeWhile (/= ' ') input
            amount = (read :: String -> Int) . tail . dropWhile (/= ' ') $ input
            command = case name of
                        "forward" -> Forward amount
                        "up" -> Up amount
                        "down" -> Down amount
                        _ -> error $ "Unrecognized name " ++ name
         in [(command, "")]

part1 :: [Command] -> (Int, Int)
part1 = foldr (\command (horiz, depth) -> case command of
                Forward x -> (horiz + x, depth)
                Up x -> (horiz, depth - x)
                Down x -> (horiz, depth + x)
              ) (0, 0)

main :: IO Int
main = do
    values <- loadValues
    let (horiz, depth) = part1 $ fmap (read :: String -> Command) values
    return $ horiz * depth