module Day08B where

import Data.Char ( digitToInt, isDigit, intToDigit )
import Data.List.Split ( chunksOf )
import Data.List ( find, intercalate  )
import Data.Maybe ( fromJust )
import Data.Function ( (&) )

type Dimensions = (Int, Int)
type Layer = [Int]

solve :: IO ()
solve = do
    input <- readFile "input/Day08.txt"
    input
        & parseInput (25, 6)
        & transpose
        & map intToDigit
        & chunksOf 25
        & intercalate "\n"
        & putStrLn

transpose :: [Layer] -> Layer
transpose layers = map firstNonTransparant pixels
    where
        pixelsPerLayer = length (head layers)
        firstNonTransparant = fromJust . find (/= 2)
        pixels = [map (!! x) layers | x <- [0..pixelsPerLayer - 1]]

parseInput :: Dimensions -> String -> [Layer]
parseInput (w, h) input = chunksOf pixelsPerLayer digits
    where
        pixelsPerLayer = w * h
        digits = map digitToInt (filter isDigit input)
