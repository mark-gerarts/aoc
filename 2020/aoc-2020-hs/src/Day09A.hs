module Day09A where

import Data.List (tails)

main :: IO ()
main = do
  input <- readFile "input/Day09.txt"
  print $ getFirstInvalid $ parseInput input

preamble :: Int
preamble = 25

getFirstInvalid :: [Int] -> Int
getFirstInvalid xs
  | length xs < preamble = error "List is smaller than preamble"
  | otherwise =
    if isValid x (take preamble xs)
      then getFirstInvalid $ tail xs
      else x
  where
    x = head $ drop preamble xs

isValid :: Int -> [Int] -> Bool
isValid n ns = not $ null [True | (x : ys) <- tails ns, y <- ys, x + y == n]

parseInput :: String -> [Int]
parseInput = map read . lines
