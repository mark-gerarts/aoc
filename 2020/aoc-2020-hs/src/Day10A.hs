module Day10A where

import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "input/Day10.txt"
  print $ solve input

solve :: String -> Int
solve input = ones * threes
  where
    diffs = differences $ parseInput input

    ones = length $ filter (== 1) diffs

    threes = length $ filter (== 3) diffs

differences :: [Int] -> [Int]
differences xs = zipWith (flip (-)) withStartAndEnd (tail withStartAndEnd)
  where
    sorted = sort xs

    withStartAndEnd = 0 : sorted ++ [last sorted + 3]

parseInput :: String -> [Int]
parseInput = map read . lines

exampleInput :: String
exampleInput =
  unlines
    [ "16",
      "10",
      "15",
      "5",
      "1",
      "11",
      "7",
      "19",
      "6",
      "12",
      "4"
    ]

exampleInput2 :: String
exampleInput2 =
  unlines
    [ "28",
      "33",
      "18",
      "42",
      "31",
      "14",
      "46",
      "20",
      "48",
      "47",
      "24",
      "23",
      "49",
      "45",
      "19",
      "38",
      "39",
      "11",
      "1",
      "32",
      "25",
      "35",
      "8",
      "17",
      "7",
      "9",
      "4",
      "2",
      "34",
      "10",
      "3"
    ]
