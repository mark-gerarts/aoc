module Day04A where

input :: (Int, Int)
input = (134792, 675810)

solve :: IO ()
solve = print numberOfValidInputs

numberOfValidInputs :: Int
numberOfValidInputs = length $ filter isValid [fst input .. snd input]

isValid :: Int -> Bool
isValid n = hasDoubleDigit n && neverDecreases n

hasDoubleDigit :: Int -> Bool
hasDoubleDigit n = any (uncurry (==)) $ zip digits (tail digits)
    where
        digits = toDigits n

neverDecreases :: Int -> Bool
neverDecreases n = all (uncurry (<=)) $ zip digits (tail digits)
    where
        digits = toDigits n

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]
