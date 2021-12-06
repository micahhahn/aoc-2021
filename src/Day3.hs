{-# LANGUAGE TypeApplications #-}

module Day3 (example, part1, part2) where

import Data.Bits
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (partition)
import Data.Semigroup (Max(..))
import Data.Word
import Control.Arrow (ArrowZero(zeroArrow))

example = 
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    ]

parser :: String -> Word64
parser = foldl' (\val digit -> val * 2 + fromIntegral (digitToInt digit)) 0

unpack :: Word64 -> [ Int ]
unpack w = fmap (\i -> fromIntegral $ shift w (-i) .&. 1) [0..63]

newtype Ior a = Ior { getIor :: a }

instance (Bits a) => Monoid (Ior a) where
    mempty = Ior zeroBits    

instance (Bits a) => Semigroup (Ior a) where
    Ior x <> Ior y = Ior (x .|. y)

part1 :: [String] -> String
part1 input = let values = fmap parser input
                  bitCount = getMax $ foldMap (Max . length) input
                  sums = foldl' (\acc item -> [ l+r | (l, r) <- zip acc item]) (unpack zeroBits) (fmap unpack values)
                  gamma = getIor $ foldMap (\(count, i) -> Ior $ if count * 2 >= length values then shift 1 i else (zeroBits :: Word64) ) (zip sums [0..63])
                  epsilon = (shift 1 bitCount - 1) .&. complement gamma
               in show $ fromIntegral @_ @Int $ gamma * epsilon

choose :: (Show a) => (Maybe Int -> Bool) -> [([Int], a)] -> Maybe a
choose filter vals = let (ones, zeros) = partition (\(s:_, _) -> s == 1) vals
                         onesCommon = if length ones > length zeros
                                      then Just 1
                                      else if length ones < length zeros
                                           then Just 0
                                           else Nothing
                         filtered = if filter onesCommon then ones else zeros
                 in case filtered of
                        [] -> Nothing
                        [(_, a)] -> Just a
                        many -> choose filter (fmap (\(_:ss, a) -> (ss, a)) many)

oxygenFilter :: Maybe Int -> Bool
oxygenFilter mostCommon = 
    case mostCommon of
        Just 1 -> True
        Just 0 -> False
        Nothing -> True

co2Filter :: Maybe Int -> Bool
co2Filter mostCommon = 
    case mostCommon of
        Just 1 -> False
        Just 0 -> True
        Nothing -> False

part2 :: [String] -> String
part2 input = let bitCount = getMax $ foldMap (Max . length) input
                  values = fmap (\i -> (drop (64 - bitCount) . reverse . unpack $ parser i, parser i)) input
                  oxygen = choose oxygenFilter values
                  co2 = choose co2Filter values
               in case (oxygen, co2) of
                      (Just w1, Just w2) -> show $ w1 * w2
                      _ -> ""
