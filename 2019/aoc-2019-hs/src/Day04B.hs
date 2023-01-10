module Day04B where

input :: (Int, Int)
input = (134792, 675810)

solve :: IO ()
solve = print numberOfValidInputs

numberOfValidInputs :: Int
numberOfValidInputs = length $ filter isValid [fst input .. snd input]

isValid :: Int -> Bool
isValid n = hasDoubleDigit n && neverDecreases n

hasDoubleDigit :: Int -> Bool
hasDoubleDigit = elem 2 . map length . groups . toDigits

-- |Groups subsequent elements that are equal to eachother. E.g.:
-- [1,1,2,3,3,3] -> [[1,1,1],[2],[3,3,3]]
groups :: Eq a => [a] -> [[a]]
groups = reverse . go []
    where
        go :: Eq a => [[a]] -> [a] -> [[a]]
        go carry [] = carry
        go [] (x:xs) = go [[x]] xs
        go (c:cs) (x:xs) =
            if x `elem` c
                then go ((x : c) : cs) xs
                else go ([x] : (c:cs)) xs

neverDecreases :: Int -> Bool
neverDecreases n = all (uncurry (<=)) $ zip digits (tail digits)
    where
        digits = toDigits n

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]
