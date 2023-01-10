module Day10B where

import qualified Data.Map as Map
import Data.List ( minimumBy )

type Point = (Int, Int)
type Segment = (Point, Point)
type AsteroidMap = Map.Map Point Bool
type Dimensions = (Int, Int)

solve :: IO ()
solve = do
    input <- readFile "input/Day10.txt"
    let
        --m = parseInput input
        -- asteroids = vaporize m (21, 16) (21, 0)
        asteroids = vaporize (parseInput example0) (8, 0) (8, 3)
    mapM_ print (zip [1..] $ take 200 asteroids)

example0 :: String
example0 = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"

vaporize :: AsteroidMap -> Point -> Point -> [Point]
vaporize m c edgePoint =
    case asteroidsWithDistance of
        [] -> vaporize m c nextEdgePoint
        _ -> let vaporizedAsteroid = snd (minimumBy (\a b -> compare (fst a) (fst b)) asteroidsWithDistance)
                in vaporizedAsteroid : vaporize (Map.insert vaporizedAsteroid False m) c nextEdgePoint
    where
        dimensions = getDimensions m
        allAsteroids = getAsteroidLocations m
        asteroidsInSight = getAsteroidsBetween allAsteroids c edgePoint
        asteroidsWithDistance = map (\p -> (distance c p, p)) asteroidsInSight
        nextEdgePoint = rotate dimensions edgePoint


-- |Rotates along the outer edges of the square, creating a segment from the
-- given center to the next point on the outer edge.
rotate :: Dimensions -> Point -> Point
rotate (w, h) (x, y)
    | y == 0 && x < (w - 1) = (x + 1, y)
    | x == (w - 1) && y < (h - 1) = (x, y + 1)
    | y == (h - 1) && x > 0 = (x - 1, y)
    | otherwise = (x, y - 1)

getDimensions :: AsteroidMap -> Dimensions
getDimensions m = (xMax + 1, yMax + 1)
    where
        (xMax, yMax) = maximum $ Map.keys m

getAsteroidsBetween :: [Point] -> Point -> Point -> [Point]
getAsteroidsBetween allAsteroids a b = filter (isBetween a b)
    $ filter (/= a) allAsteroids

-- (21, 16)
findMonitoringStationLocation :: AsteroidMap -> Point
findMonitoringStationLocation m =
    fst $ minimumBy (\a b -> compare (snd a) (snd b)) locations
    where
        asteroidLocations = getAsteroidLocations m
        locations = map (\p -> (p, getNumberOfVisibleAsteroids p asteroidLocations)) asteroidLocations

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
