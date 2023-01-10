module Day04B where

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
isValid passport =
  all (has passport) requiredFields && all isDataValid passport
  where
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- | Yay for regexes
isDataValid :: (String, String) -> Bool
isDataValid ("byr", birthYear) = read birthYear >= 1920 && read birthYear <= 2002
isDataValid ("iyr", issueYear) = read issueYear >= 2010 && read issueYear <= 2020
isDataValid ("eyr", expirationYear) = read expirationYear >= 2020 && read expirationYear <= 2030
isDataValid ("hgt", height) = validHeight height
isDataValid ("hcl", color) = color =~ "^#[0-9a-f]{6}$"
isDataValid ("ecl", color) = color =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"
isDataValid ("pid", id) = id =~ "^[0-9]{9}$"
isDataValid _ = True

validHeight :: String -> Bool
validHeight input = case unit of
  "in" -> height >= 59 && height <= 76
  "cm" -> height >= 150 && height <= 193
  _ -> False
  where
    heightString = input =~ "[0-9]+"

    height = read heightString

    unit = input =~ "(in|cm)"

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
