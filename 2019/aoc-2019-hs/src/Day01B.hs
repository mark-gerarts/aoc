module Day01B ( solve ) where

type Mass = Int

solve :: IO ()
solve = do
    input <- readFile "input/Day01.txt"
    let masses = (map read $ lines input) :: [Int]
        totalFuelRequired = calculateTotalRequiredFuel masses
    print totalFuelRequired

calculateRequiredFuel :: Mass -> Int
calculateRequiredFuel = sum . takeWhile (> 0) . fuelCounts

fuelCounts :: Mass -> [Int]
fuelCounts mass =
    let fuelCount = calculateFuelForMass mass
    in (fuelCount : fuelCounts fuelCount)

calculateFuelForMass :: Mass -> Int
calculateFuelForMass mass = (mass `div` 3) - 2

calculateTotalRequiredFuel :: [Mass] -> Int
calculateTotalRequiredFuel = sum . map calculateRequiredFuel
