{-# LANGUAGE RecordWildCards #-}

module Day08B where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S

data Operation = Acc Int | Jmp Int | Nop Int deriving (Show)

type Program = IntMap Operation

data State = State
  { acc :: Int,
    line :: Int,
    visitedLineNumbers :: Set Int,
    loopDetected :: Bool
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "input/Day08.txt"
  undefined

initialState :: State
initialState =
  State
    { acc = 0,
      line = 0,
      visitedLineNumbers = S.empty,
      loopDetected = False
    }

fixProgram :: Program -> Program
fixProgram p = go p 0
  where
    testWithNewInstruction is p lineNumber =
      let newProgram = IM.insert lineNumber is p
       in if hasLoop newProgram
            then go p (lineNumber + 1)
            else newProgram

    go :: Program -> Int -> Program
    go p lineNumber = case p IM.! lineNumber of
      Acc _ -> go p (lineNumber + 1)
      Nop v -> testWithNewInstruction (Jmp v) p lineNumber
      Jmp v -> testWithNewInstruction (Nop v) p lineNumber

hasLoop :: Program -> Bool
hasLoop p = go p initialState
  where
    go p s
      | loopDetected nextState = True
      | finished p s = False
      | otherwise = go p nextState
      where
        nextState = step p s

        finished p s = IM.notMember (line s) p

step :: Program -> State -> State
step p s =
  State
    { acc = acc nextState,
      line = line nextState,
      visitedLineNumbers = S.insert (line s) (visitedLineNumbers s),
      loopDetected = S.member (line nextState) (visitedLineNumbers s)
    }
  where
    currentOp = p IM.! line s
    nextState = execute currentOp s

execute :: Operation -> State -> State
execute (Acc v) State {..} = State {line = line + 1, acc = acc + v, ..}
execute (Jmp v) State {..} = State {line = line + v, ..}
execute (Nop _) State {..} = State {line = line + 1, ..}

parseInput :: String -> Program
parseInput = IM.fromList . zip [0 ..] . map parseLine . lines

parseLine :: String -> Operation
parseLine line =
  case take 3 line of
    "acc" -> Acc value
    "jmp" -> Jmp value
    "nop" -> Nop value
    _ -> error $ "Unrecognized instruction: " ++ take 3 line
  where
    -- Basically a really dumb way to parse this. `read` throws an error on the
    -- + sign, so we strip it away.
    value =
      if line !! 4 == '+'
        then read $ drop 5 line
        else read $ drop 4 line

exampleInput :: String
exampleInput =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]
