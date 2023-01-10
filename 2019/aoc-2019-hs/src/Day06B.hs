module Day06B where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Data.List ( intersect )

type OrbitalMap = Map.Map Object Object
data Object = CenterOfMass | Object String deriving ( Show, Eq )

instance Ord Object where
    (Object x) <= (Object y) = x <= y
    CenterOfMass <= (Object _) = True
    (Object _) <= CenterOfMass = False


solve :: IO ()
solve = do
    input <- readFile "input/Day06.txt"
    let orbitalMap = parseInput input
        distance = calculateDistanceBetween orbitalMap (Object "YOU") (Object "SAN")
    print distance

sampleInput :: String
sampleInput = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

calculateDistanceBetween :: OrbitalMap -> Object -> Object -> Int
calculateDistanceBetween orbitalMap a b =
    length listA + length listB - 2 * length (listA `intersect` listB)
    where
        listA = getOrbitList orbitalMap a
        listB = getOrbitList orbitalMap b

getOrbitList :: OrbitalMap -> Object -> [Object]
getOrbitList _ CenterOfMass = []
getOrbitList orbitalMap object =  next : getOrbitList orbitalMap next
    where
        next = (Map.!) orbitalMap object

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
