module Day01A where

import Data.List (find, tails)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "input/Day01.txt"
  print $ solve $ parseInput input

solve :: [Integer] -> Integer
solve = uncurry (*) . fromJust . find (\(x, y) -> x + y == 2020) . twoPairs

twoPairs :: [a] -> [(a, a)]
twoPairs [] = []
twoPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

parseInput :: String -> [Integer]
parseInput = map read . lines
