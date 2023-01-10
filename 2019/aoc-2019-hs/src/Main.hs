module Main where

import Day01A ( solve )
import Day01B ( solve )
import Day02A ( solve )
import Day02B ( solve )
import Day03A ( solve )
import Day03B ( solve )
import Day04A ( solve )
import Day04B ( solve )
import Day05A ( solve )
import Day05B ( solve )
import Day06A ( solve )
import Day06B ( solve )
import Day07A ( solve )
import Day07B ( solve )
import Day08A ( solve )
import Day08B ( solve )
import Day10A ( solve )
import Day10B ( solve )
import Day12A ( solve )
import Day14A ( solve )
import System.Environment ( getArgs )

type ArgumentList = [String]

main :: IO ()
main = do
  args <- getArgs
  solveDay $ getDay args

getDay :: ArgumentList -> String
getDay (x:_) = x
getDay [] = error "Please provide a module name. Usage: stack run Day01B"

solveDay :: String -> IO ()
solveDay "Day01A" = Day01A.solve
solveDay "Day01B" = Day01B.solve
solveDay "Day02A" = Day02A.solve
solveDay "Day02B" = Day02B.solve
solveDay "Day03A" = Day03A.solve
solveDay "Day03B" = Day03B.solve
solveDay "Day04A" = Day04A.solve
solveDay "Day04B" = Day04B.solve
solveDay "Day05A" = Day05A.solve
solveDay "Day05B" = Day05B.solve
solveDay "Day06A" = Day06A.solve
solveDay "Day06B" = Day06B.solve
solveDay "Day07A" = Day07A.solve
solveDay "Day07B" = Day07B.solve
solveDay "Day08A" = Day08A.solve
solveDay "Day08B" = Day08B.solve
solveDay "Day10A" = Day10A.solve
solveDay "Day10B" = Day10B.solve
solveDay "Day12A" = Day12A.solve
solveDay "Day14A" = Day14A.solve
solveDay input = error $ "No module found for " ++ input
