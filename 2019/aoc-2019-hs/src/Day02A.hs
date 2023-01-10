module Day02A ( solve ) where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Data.Function ((&))

data Program = Program { instructionSet :: InstructionSet
                       , pointer :: Position
                       , state :: State }
                       deriving ( Show )
type InstructionSet = Map.Map Position IntCode
type Position = Int
type IntCode = Int
data State = Executing | Halted deriving ( Show )

solve :: IO ()
solve = do
    input <- readFile "input/Day02.txt"
    input
        & parseInput
        & setIntCodeAt 1 12
        & setIntCodeAt 2 2
        & run
        & getIntCodeAt 0
        & print

parseInput :: String -> Program
parseInput input = makeProgram $ Map.fromList $ zip [0..] ints
    where
        ints :: [Int]
        ints = map read $ splitOn "," input

makeProgram :: InstructionSet -> Program
makeProgram instructionSet =
    Program { instructionSet = instructionSet
            , pointer = 0
            , state = Executing }

run :: Program -> Program
run program@Program{state = Halted} = program
run program = run $ step program

step :: Program -> Program
step program = execute intcode program
    where
        intcode = (Map.!) (instructionSet program) (pointer program)

execute :: IntCode -> Program -> Program
execute 1 = add
execute 2 = mult
execute 99 = halt
execute _ = error "Invalid IntCode"

executeSimpleOp :: (Int -> Int -> Int) -> Program -> Program
executeSimpleOp f program =
    let currentPointer = pointer program
        argA = getIntCodeAt (currentPointer + 1) program
        argB = getIntCodeAt (currentPointer + 2) program
        argC = getIntCodeAt (currentPointer + 3) program
        result = f (getIntCodeAt argA program) (getIntCodeAt argB program)
        newProgram = setIntCodeAt argC result program
    in advancePointer newProgram 4

add :: Program -> Program
add = executeSimpleOp (+)

mult :: Program -> Program
mult = executeSimpleOp (*)

halt :: Program -> Program
halt p = p { state = Halted }

getIntCodeAt :: Position -> Program -> IntCode
getIntCodeAt i p = (Map.!) (instructionSet p) i

setIntCodeAt :: Position -> IntCode -> Program -> Program
setIntCodeAt i v p = p { instructionSet = Map.insert i v (instructionSet p) }

advancePointer :: Program -> Int -> Program
advancePointer p n = p { pointer = pointer p + n }
