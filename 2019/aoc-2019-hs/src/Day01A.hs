module Day01A ( solve ) where

type Mass = Int

solve :: IO ()
solve = do
    input <- readFile "input/Day01.txt"
    let masses = (map read $ lines input) :: [Int]
        totalFuelRequired = calculateTotalRequiredFuel masses
    print totalFuelRequired

calculateRequiredFuel :: Mass -> Int
calculateRequiredFuel mass = (mass `div` 3) - 2

calculateTotalRequiredFuel :: [Mass] -> Int
calculateTotalRequiredFuel = sum . map calculateRequiredFuel
