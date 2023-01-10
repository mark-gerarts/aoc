module Day17A where

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set

data State = Active | Inactive deriving (Show, Eq)

type Position = (Int, Int, Int, Int)

type Grid = Set Position

-- It's on the slow side (~20s), but it works.
main :: IO ()
main = do
  input <- readFile "input/Day17.txt"
  print $ solve input

solve :: String -> Int
solve = countActives . (!! 6) . iterate step . parseInput

countActives :: Grid -> Int
countActives = Set.size

step :: Grid -> Grid
step g = foldr foldfn Set.empty positionsToCheck
  where
    setPositions = Set.toList g

    positionsToCheck = nub (setPositions ++ concatMap getNeighbours setPositions)

    foldfn p g' = if getNextState g p == Active then Set.insert p g' else g'

getNextState :: Grid -> Position -> State
getNextState g p = case currentState of
  Active ->
    if activeNeighbours == 2 || activeNeighbours == 3
      then Active
      else Inactive
  Inactive -> if activeNeighbours == 3 then Active else Inactive
  where
    activeNeighbours = getNumberOfActiveNeighbours g p

    currentState = getState g p

getNumberOfActiveNeighbours :: Grid -> Position -> Int
getNumberOfActiveNeighbours g p = length $ filter (== Active) $ getNeighbourStates g p

getNeighbourStates :: Grid -> Position -> [State]
getNeighbourStates g p = map (getState g) (getNeighbours p)

getState :: Grid -> Position -> State
getState g p = if Set.member p g then Active else Inactive

add :: Position -> Position -> Position
add (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

getNeighbours :: Position -> [Position]
getNeighbours p = map (add p) increments
  where
    increments =
      [ (x, y, z, w)
        | x <- [-1, 0, 1],
          y <- [-1, 0, 1],
          z <- [-1, 0, 1],
          w <- [-1, 0, 1],
          not $ x == 0 && y == 0 && z == 0 && w == 0
      ]

parseInput :: String -> Grid
parseInput i =
  Set.fromList
    [ (x, y, 0, 0)
      | (y, rows) <- zip [0 ..] (lines i),
        (x, c) <- zip [0 ..] rows,
        parseState c == Active
    ]

parseState :: Char -> State
parseState '#' = Active
parseState _ = Inactive

exampleInput :: String
exampleInput =
  unlines
    [ ".#.",
      "..#",
      "###"
    ]
