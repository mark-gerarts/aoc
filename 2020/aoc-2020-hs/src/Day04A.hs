module Day04A where

import Data.List.Split (splitOn)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

type Passport = [(String, String)]

main :: IO ()
main = do
  input <- readFile "input/Day04.txt"
  print $ solve input

solve :: String -> Int
solve = length . filter isValid . parseInput

isValid :: Passport -> Bool
isValid passport = all (has passport) requiredFields
  where
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

has :: Passport -> String -> Bool
has [] _ = False
has ((key', _) : xs) key = key' == key || has xs key

parseInput :: String -> [Passport]
parseInput = map parsePassport . splitOn "\n\n"

parsePassport :: String -> Passport
parsePassport input = toTuples matches
  where
    matches = getAllTextMatches (input =~ "[^: \n]+")

    toTuples [] = []
    toTuples (x : y : xs) = (x, y) : toTuples xs
