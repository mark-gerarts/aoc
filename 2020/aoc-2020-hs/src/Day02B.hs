module Day02B where

import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

type Policy = (Char, Int, Int)

type Password = String

main :: IO ()
main = do
  input <- readFile "input/Day02.txt"
  print $ solve input

solve :: String -> Int
solve = length . filter (uncurry isValid) . parseInput

isValid :: Policy -> Password -> Bool
isValid (char, i1, i2) password =
  case (charAt i1 == char, charAt i2 == char) of
    (True, False) -> True
    (False, True) -> True
    _ -> False
  where
    charAt i = password !! (i - 1)

-- I should really learn parsing in Haskell :^)
parseLine :: String -> (Policy, Password)
parseLine line = ((character, read i1, read i2), password)
  where
    [i1, i2] = getAllTextMatches (line =~ "[0-9]+") :: [String]
    [character : _, password] = getAllTextMatches (line =~ "[a-z]+") :: [String]

parseInput :: String -> [(Policy, Password)]
parseInput = map parseLine . lines
