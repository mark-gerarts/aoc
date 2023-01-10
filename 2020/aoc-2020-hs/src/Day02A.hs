module Day02A where

import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

type Policy = (Char, Int, Int)

type Password = String

main :: IO ()
main = do
  input <- readFile "input/Day02.txt"
  print $ solve input

solve :: String -> Int
solve = length . filter (uncurry isValid) . parseInput

isValid :: Policy -> Password -> Bool
isValid (char, min, max) password = occurences >= min && occurences <= max
  where
    occurences = length $ filter (== char) password

-- I should really learn parsing in Haskell :^)
parseLine :: String -> (Policy, Password)
parseLine line = ((character, read min, read max), password)
  where
    [min, max] = getAllTextMatches (line =~ "[0-9]+") :: [String]
    [character : _, password] = getAllTextMatches (line =~ "[a-z]+") :: [String]

parseInput :: String -> [(Policy, Password)]
parseInput = map parseLine . lines
