{-# LANGUAGE TypeApplications #-}

module Day3 (example, part1, part2) where

import Data.Bits
import Data.Char (digitToInt)
import Data.Foldable (foldl')
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

part2 :: [String] -> String
part2 = const ""
