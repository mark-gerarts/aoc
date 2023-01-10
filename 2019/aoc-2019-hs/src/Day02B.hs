module Day02B ( solve ) where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Data.Function ( (&) )
import Data.Maybe ( isJust , fromJust )
import Data.Foldable ( find )

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
        & getNounAndVerbForResult 19690720
        & \(n,v) -> 100 * n + v
        & print


getNounAndVerbForResult :: Int -> Program -> (Int, Int)
getNounAndVerbForResult result program =
    fromJust $ head $ filter isJust $ map (uncurry getNounAndVerb) pairs
    where
        pairs = concat [ [(x,y), (y,x)] | y <- [1..], x <- [1..y]]
        getNounAndVerb n v | getResultForNounAndVerb n v program == result = Just (n,v)
                           | otherwise = Nothing

getResultForNounAndVerb :: Int -> Int -> Program -> Int
getResultForNounAndVerb noun verb =
    getIntCodeAt 0 . run . setIntCodeAt 1 noun . setIntCodeAt 2 verb

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
