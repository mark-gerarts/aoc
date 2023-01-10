module Day08A where

import Data.Char ( digitToInt, isDigit )
import Data.List.Split ( chunksOf )
import Data.List ( minimumBy )
import Data.Function ( (&) )

type Dimensions = (Int, Int)
type Layer = [Int]

solve :: IO ()
solve = do
    input <- readFile "input/Day08.txt"
    input
        & parseInput (25, 6)
        & getCorruptionCheckDigit
        & print

getCorruptionCheckDigit :: [Layer] -> Int
getCorruptionCheckDigit layers = numOnes * numTwos
    where
        numberOfZeros = map (countOccurences 0) layers
        (_, layer) = minimumBy (\a b -> compare (fst a) (fst b)) $ zip numberOfZeros layers
        numOnes = countOccurences 1 layer
        numTwos = countOccurences 2 layer

countOccurences :: Eq a => a -> [a] -> Int
countOccurences x = length . filter (== x)

parseInput :: Dimensions -> String -> [Layer]
parseInput (w, h) input = chunksOf pixelsPerLayer digits
    where
        pixelsPerLayer = w * h
        digits = map digitToInt (filter isDigit input)
