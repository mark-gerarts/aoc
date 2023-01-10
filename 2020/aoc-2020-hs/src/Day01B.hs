module Day01B where

import Data.List (find, tails)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "input/Day01.txt"
  print $ solve $ parseInput input

-- As opposed to part 1, we put everything in one list expression now. It might
-- be slightly inefficient, but hey, it works!
solve :: [Integer] -> Integer
solve xs = head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

parseInput :: String -> [Integer]
parseInput = map read . lines
