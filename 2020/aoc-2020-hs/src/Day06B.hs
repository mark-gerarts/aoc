module Day06B where

import Data.List (intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input/Day06.txt"
  print $ solve input

solve :: String -> Int
solve = sum . map (length . foldr1 intersect) . parseInput

parseInput :: String -> [[String]]
parseInput = map lines . splitOn "\n\n"
