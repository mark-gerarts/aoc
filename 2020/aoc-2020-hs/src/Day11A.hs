module Day11A where

import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)

data State = Empty | Occupied | Floor deriving (Show, Eq)

type Position = (Int, Int)

type Grid = Map Position State

countOccupiedSeats :: Grid -> Int
countOccupiedSeats g = Map.size $ Map.filter (== Occupied) g

stepUntilStabilized :: Grid -> Grid
stepUntilStabilized g
  | step g == g = g
  | otherwise = stepUntilStabilized (step g)

step :: Grid -> Grid
step g = Map.mapWithKey (\k _ -> nextState g k) g

nextState :: Grid -> Position -> State
nextState g p = case state of
  Floor -> Floor
  Empty ->
    if occupiedNeighbours == 0
      then Occupied
      else Empty
  Occupied -> if occupiedNeighbours >= 4 then Empty else Occupied
  where
    state = fromJust $ get g p
    neighbours = getNeighbours g p
    occupiedNeighbours = length $ filter (== Occupied) neighbours

getNeighbours :: Grid -> Position -> [State]
getNeighbours g = mapMaybe (get g) . neighbours
  where
    neighbours (x, y) =
      [ (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 && dy /= 0
      ]

get :: Grid -> Position -> Maybe State
get g p = Map.lookup p g

-- For some visual debugging
printGrid :: Grid -> String
printGrid g = unlines $ chunksOf (maxX + 1) $ map (toChar . (Map.!) g) ps
  where
    ((maxX, maxY), _) = Map.findMax g

    ps = [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]]

parseInput :: String -> Grid
parseInput i =
  Map.fromList
    [ ((x, y), parseChar state)
      | (y, states) <- zip [0 ..] $ lines i,
        (x, state) <- zip [0 ..] states
    ]

parseChar :: Char -> State
parseChar 'L' = Empty
parseChar '#' = Occupied
parseChar '.' = Floor
parseChar _ = error "Undefined character"

toChar :: State -> Char
toChar Occupied = '#'
toChar Empty = 'L'
toChar Floor = '.'

exampleInput :: String
exampleInput =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]
