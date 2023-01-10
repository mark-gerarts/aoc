module Day05A where

main :: IO ()
main = do
  input <- readFile "input/Day05.txt"
  print $ solve input

solve :: String -> Int
solve = maximum . map getSeatId . lines

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
