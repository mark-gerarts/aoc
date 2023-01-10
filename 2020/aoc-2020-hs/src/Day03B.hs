module Day03B where

type Slope = (Int, Int)

type Grid = [String]

main :: IO ()
main = do
  input <- readFile "input/Day03.txt"
  print $ solve input

solve :: String -> Int
solve input = product $ map (\s -> getNumberOfTreesForSlope s grid) slopes
  where
    grid = parseInput input
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

getNumberOfTreesForSlope :: Slope -> Grid -> Int
getNumberOfTreesForSlope s = length . filter (== '#') . collectPath s

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
