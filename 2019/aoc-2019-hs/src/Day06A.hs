module Day06A where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )

type OrbitalMap = Map.Map Object Object
data Object = CenterOfMass | Object String deriving ( Show, Eq )

instance Ord Object where
    (Object x) <= (Object y) = x <= y
    CenterOfMass <= (Object _) = True
    (Object _) <= CenterOfMass = False


solve :: IO ()
solve = do
    input <- readFile "input/Day06.txt"
    print $ countTotalOrbits $ parseInput input

countTotalOrbits :: OrbitalMap -> Int
countTotalOrbits orbitalMap =
    foldl (\c obj -> c + countOrbits orbitalMap obj) 0 (Map.keys orbitalMap)

countOrbits :: OrbitalMap -> Object -> Int
countOrbits _ CenterOfMass = 0
countOrbits orbitalMap object = 1 + countOrbits orbitalMap ((Map.!) orbitalMap object)

parseInput :: String -> OrbitalMap
parseInput = foldl (\orbitalMap (a, b) -> orbits orbitalMap b a) Map.empty
                . map parseLine
                . lines

orbits :: OrbitalMap -> Object -> Object -> OrbitalMap
orbits map a b = Map.insert a b map

parseObject :: String -> Object
parseObject "COM" = CenterOfMass
parseObject id = Object id

parseLine :: String -> (Object, Object)
parseLine line = (a, b)
    where
        [a, b] = map parseObject $ splitOn ")" line
