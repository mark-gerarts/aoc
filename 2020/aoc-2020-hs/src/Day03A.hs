module Day03A where

type Slope = (Int, Int)

type Grid = [String]

main :: IO ()
main = do
  input <- readFile "input/Day03.txt"
  print $ solve input

solve :: String -> Int
solve = length . filter (== '#') . collectPath (3, 1) . parseInput

collectPath :: Slope -> Grid -> String
collectPath (right, down) grid = go (0, 0) ""
  where
    go (x, y) path
      | y >= length grid = path
      | otherwise = go (x + right, y + down) (char : path)
      where
        char = (grid !! y) !! x

parseInput :: String -> Grid
parseInput = map cycle . lines
