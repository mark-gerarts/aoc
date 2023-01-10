{-# LANGUAGE RecordWildCards #-}

module Day12A where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Prelude hiding (Left, Right)

type Position = (Int, Int)

data Direction = North | South | East | West | Forward deriving (Show, Eq)

data TurnDirection = Left | Right deriving (Show)

data Action = Move (Direction, Int) | Turn (TurnDirection, Int) deriving (Show)

data State = State
  { direction :: Direction,
    position :: Position
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "input/Day12.txt"
  print $ solve input

solve :: String -> Int
solve i = abs x + abs y
  where
    finalState = foldl perform initialState (parseInput i)
    (x, y) = position finalState

initialState :: State
initialState = State {position = (0, 0), direction = East}

perform :: State -> Action -> State
perform State {..} (Move (dir, value)) =
  let newPosition = case dir of
        Forward -> move position direction value
        _ -> move position dir value
   in State {position = newPosition, ..}
perform State {..} (Turn (dir, value)) =
  State {direction = turn direction dir value, ..}

move :: Position -> Direction -> Int -> Position
move (x, y) direction steps = case direction of
  North -> (x, y - steps)
  South -> (x, y + steps)
  East -> (x - steps, y)
  West -> (x + steps, y)

turn :: Direction -> TurnDirection -> Int -> Direction
turn currentDir Left degrees = turn currentDir Right (360 - degrees)
turn currentDir Right degrees = dirs !! (currentIndex + steps)
  where
    steps = degrees `div` 90

    dirs = cycle [North, East, South, West]

    currentIndex = fromJust $ elemIndex currentDir dirs

parseInput :: String -> [Action]
parseInput = map parseLine . lines

parseLine :: String -> Action
parseLine l = case head l of
  'N' -> Move (North, value)
  'S' -> Move (South, value)
  'E' -> Move (East, value)
  'W' -> Move (West, value)
  'F' -> Move (Forward, value)
  'L' -> Turn (Left, value)
  'R' -> Turn (Right, value)
  where
    value = read $ tail l

exampleInput :: String
exampleInput =
  unlines
    [ "F10",
      "N3",
      "F7",
      "R90",
      "F11"
    ]
