module Day2 (part1, part2, example) where

import Data.List (foldl')

example = 
    [ "forward 5"
    , "down 5"
    , "forward 8"
    , "up 3"
    , "down 8"
    , "forward 2"
    ]

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

parse :: String -> Command
parse = read

part1 :: [String] -> String
part1 input = let values = fmap parse input 
                  (horiz, depth) = foldr (\command (horiz, depth) -> case command of
                                        Forward x -> (horiz + x, depth)
                                        Up x -> (horiz, depth - x)
                                        Down x -> (horiz, depth + x)
                                    ) (0, 0) values
               in show $ horiz * depth

data State2 = State2 { horiz :: Int, depth :: Int, aim :: Int }
    deriving (Show)

part2 :: [String] -> String
part2 input = let values = fmap parse input
                  (State2 horiz depth _) = 
                    foldl' (\(State2 horiz depth aim) command -> case command of
                        Forward x -> State2 (horiz + x) (depth + (aim * x)) aim 
                        Up x -> State2 horiz depth (aim - x)
                        Down x -> State2 horiz depth (aim + x)
                    ) (State2 0 0 0) values
               in show $ horiz * depth 