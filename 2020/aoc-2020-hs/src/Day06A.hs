module Day06A where

import Data.List (nub)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input/Day06.txt"
  print $ solve input

solve :: String -> Int
solve = sum . map (length . nub . concat) . parseInput

parseInput :: String -> [[String]]
parseInput = map lines . splitOn "\n\n"
