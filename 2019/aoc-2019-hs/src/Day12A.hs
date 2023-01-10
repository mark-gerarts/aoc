module Day12A where

import Text.Regex.TDFA

type Vector = (Int, Int, Int)
type Position = Vector
type Velocity = Vector
type Moon = (Position, Velocity)

solve :: IO ()
solve = do
    input <- readFile "input/Day12.txt"
    print $ totalEnergy $ stepX 1000 $ parseInput input

sampleInput :: String
sampleInput = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"

stepX :: Int -> [Moon] -> [Moon]
stepX 1 ms = step ms
stepX n ms = step $ stepX (n - 1) ms

step :: [Moon] -> [Moon]
step ms = map stepSingleWithOtherMoons ms
    where
        stepSingleWithOtherMoons m = stepSingle (filter (/= m) ms) m

stepSingle :: [Moon] -> Moon -> Moon
stepSingle ms m = (applyVelocity . applyGravity gravity) m
    where
        gravity = calculateGravity (map fst ms) (fst m)


applyGravity :: Velocity -> Moon -> Moon
applyGravity g (p, v)= (p, sumVector v g)

applyVelocity :: Moon -> Moon
applyVelocity (p, v) = (sumVector p v, v)

calculateGravity :: [Position] -> Position -> Velocity
calculateGravity ms m = foldl1 sumVector $ map (comparePositions m) ms

comparePositions :: Position -> Position -> Vector
comparePositions (x, y, z) (x', y', z') = (cmp x x', cmp y y', cmp z z')
    where
        cmp a b
            |  a < b = 1
            |  a > b = -1
            | otherwise = 0

sumVector :: Vector -> Vector -> Vector
sumVector (x, y, z) (x', y', z') = (x + x', y + y', z + z')

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map moonEnergy

moonEnergy :: Moon -> Int
moonEnergy (p, v) = energy p * energy v

energy :: Vector -> Int
energy (x, y, z) = abs x + abs y + abs z

parseInput :: String -> [Moon]
parseInput s = map (\p -> (p, (0, 0, 0))) positions
    where
        positions = map parseLine (lines s)

parseLine :: String -> (Int, Int, Int)
parseLine s = (x, y, z)
    where
        digits = getAllTextMatches (s =~ "-?[0-9]+") :: [String]
        [x, y, z] = map read digits :: [Int]
