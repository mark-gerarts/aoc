module Day03A where

import Prelude hiding ( Right , Left )
import Data.List.Split ( splitOn )
import Data.List ( intersect )
import Data.Maybe ()

data Direction = Up | Right | Down | Left deriving ( Show, Eq )
type Point = ( Int, Int )
type Segment = ( Point, Point )
type Step = ( Direction, Int )
type Wire = [ Segment ]

solve :: IO ()
solve = do
    input <- readFile "input/Day03.txt"
    let (w1, w2) = parseInput input
    print $ getClosestIntersectionDistance w1 w2

sampleInput :: String
sampleInput = "R8,U5,L5,D3\nU7,R6,D4,L4"

getClosestIntersectionDistance :: Wire -> Wire -> Int
getClosestIntersectionDistance w1 w2 = minimum distances
    where
        intersects = getIntersects w1 w2
        distances = map (distance (0, 0)) intersects

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getIntersects :: Wire -> Wire -> [ Point ]
getIntersects w1 w2 =
    filter (/= (0, 0)) $ concat [ getIntersectionPoints s1 s2 | s1 <- w1, s2 <- w2 ]

getIntersectionPoints :: Segment -> Segment -> [ Point ]
getIntersectionPoints s1 s2 = getPoints s1 `intersect` getPoints s2

getPoints :: Segment -> [ Point ]
getPoints ((x1, y1), (x2, y2))
    | x1 == x2 && y1 <= y2 = [(x1, y) | y <- [y1..y2]]
    | x1 == x2 && y1 > y2 = [(x1, y) | y <- [y2..y1]]
    | y1 == y2 && x1 <= x2 = [(x, y1) | x <- [x1..x2]]
    | y1 == y2 && x1 > x2 = [(x, y1) | x <- [x2..x1]]

-- Parsing

parseInput :: String -> ( Wire, Wire )
parseInput input = ( w1, w2 )
    where
        [ w1, w2 ] = map parseWire . lines $ input

parseWire :: String -> Wire
parseWire input = zip points (tail points)
    where
        steps = map parseStep $ splitOn "," input
        points = stepsToPoints (0, 0) steps

parseDirection :: String -> Direction
parseDirection "U" = Up
parseDirection "R" = Right
parseDirection "D" = Down
parseDirection "L" = Left
parseDirection x = error $ "Invalid direction: " ++ x

parseStep :: String -> Step
parseStep s = ( direction, amount )
    where
        direction = parseDirection [head s]
        amount = read $ tail s

applyStep :: Point -> Step -> Point
applyStep (x, y) (direction, amount)
    | direction == Up = (x, y - amount)
    | direction == Down = (x, y + amount)
    | direction == Left = (x - amount, y)
    | direction == Right = (x + amount, y)

stepsToPoints :: Point -> [ Step ] -> [ Point ]
stepsToPoints p (step:steps) = p : stepsToPoints (applyStep p step) steps
stepsToPoints p [] = [p]
