module Day05B where

import Data.List ((\\))

main :: IO ()
main = do
  input <- readFile "input/Day05.txt"
  print $ solve input

allIds :: [Int]
allIds = [0 .. 127 * 8 + 7]

-- | I kinda cheat here... By printing all remaining seat IDs it is really easy
-- to visually see which one is my seat, since it is in between the lowest and
-- the highest numbers. Way faster than programming a "real" solution :^)
solve :: String -> [Int]
solve input = allIds \\ seatIds
  where
    seatIds = map getSeatId $ lines input

getSeatId :: String -> Int
getSeatId instructions = row * 8 + col
  where
    (rowInstructions, colInstructions) = splitAt 7 instructions
    row = binaryPartition rowInstructions [0 .. 127]
    col = binaryPartition colInstructions [0 .. 7]

splitInstructions :: String -> (String, String)
splitInstructions = splitAt 7

-- | Since we know the input is valid, we just partition using either `take` or
-- `drop` for each instruction.
binaryPartition :: String -> [Int] -> Int
binaryPartition [] xs = head xs
binaryPartition (i : is) xs =
  binaryPartition is $ partitionFn (length xs `div` 2) xs
  where
    partitionFn = if i `elem` "FL" then take else drop
