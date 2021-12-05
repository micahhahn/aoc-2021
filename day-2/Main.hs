module Main where

import Data.List (foldl')
import Data.Functor ((<&>))
import Lib
import System.IO
import qualified Paths_aoc as Paths

loadCommands = Paths.getDataFileName "day-2/input.txt" >>= readFile <&> fmap (read :: String -> Command) . lines

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

data State2 = State2 { horiz :: Int, depth :: Int, aim :: Int }
    deriving (Show)

part2 :: [Command] -> State2
part2 = foldl' (\(State2 horiz depth aim) command -> case command of
                Forward x -> State2 (horiz + x) (depth + (aim * x)) aim 
                Up x -> State2 horiz depth (aim - x)
                Down x -> State2 horiz depth (aim + x)
              ) (State2 0 0 0)

example = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

main :: IO Int
main = do
    commands <- loadCommands
    let (horiz, depth) = part1 commands
    let result1 = horiz * depth
    let (State2 horiz2 depth2 aim2) = part2 commands
    let result2 = horiz2 * depth2
    return $ result2