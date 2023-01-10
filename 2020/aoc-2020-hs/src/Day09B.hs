module Day09A where

import Data.List (tails)

main :: IO ()
main = do
  input <- readFile "input/Day09.txt"
  print $ solve input

preamble :: Int
preamble = 25

solve :: String -> Int
solve input = min + max
  where
    parsedInput = parseInput input

    targetRange = getTargetRange parsedInput (getFirstInvalid parsedInput)

    (min, max) = (minimum targetRange, maximum targetRange)

getTargetRange :: [Int] -> Int -> [Int]
getTargetRange xs x = go xs []
  where
    go :: [Int] -> [Int] -> [Int]
    go xs currentRange
      | sum currentRange > x = go (tail xs) []
      | sum currentRange == x && length currentRange >= 2 = currentRange
      | otherwise = go xs (take (length currentRange + 1) xs)

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
