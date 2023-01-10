module Day10A where

import qualified Data.Map as Map

type Point = (Int, Int)
type Segment = (Point, Point)
type AsteroidMap = Map.Map Point Bool

solve :: IO ()
solve = do
    input <- readFile "input/Day10.txt"
    print $ findMaxVisibleAsteroids $parseInput input

example0 :: String
example0 = ".#..#\n.....\n#####\n....#\n...##"

example1 :: String
example1 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"

example2 :: String
example2 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."

example3 :: String
example3 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."

findMaxVisibleAsteroids :: AsteroidMap -> Int
findMaxVisibleAsteroids m =
    maximum (map (\p -> getNumberOfVisibleAsteroids p asteroidLocations) asteroidLocations) - 1
    where
        asteroidLocations = getAsteroidLocations m

getNumberOfVisibleAsteroids :: Point -> [Point] -> Int
getNumberOfVisibleAsteroids a as = length $ filter (not . hasAsteroidsBetween as a) as

getAsteroidLocations :: AsteroidMap -> [Point]
getAsteroidLocations = Map.keys . Map.filter (== True)

hasAsteroidsBetween :: [Point] -> Point -> Point -> Bool
hasAsteroidsBetween allAsteroids a b = any (isBetween a b)
    $ filter (/= a) $ filter (/= b) allAsteroids

parseInput :: String -> AsteroidMap
parseInput input =
    Map.fromList [((x, y), asteroid == '#') |
        (y, asteroidList) <- zip [0..] asteroidLists,
        (x, asteroid) <- zip [0..] asteroidList ]
    where
        asteroidLists = lines input

isBetween :: Point -> Point -> Point -> Bool
isBetween s1 s2 p = abs (distA + distB - distC) <= epsilon
    where
        distA = distance p s1
        distB = distance p s2
        distC = distance s1 s2
        epsilon = 0.000001

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2
