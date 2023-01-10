module Day05B where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Data.Function ( (&) )

data Computer = Computer { program :: Program
                         , pointer :: Int
                         , state :: State
                         , input :: Int
                         , output :: [Int] } deriving ( Show )

data Instruction = Instruction { modes :: [Mode]
                               , numberOfArguments :: Int
                               , apply :: Computer -> [Parameter] -> Computer }

type Opcode = Computer -> [Parameter] -> Computer

type Program = Map.Map Int Int
data Mode = Position | Immediate deriving ( Show )
data State = Running | Halted deriving ( Show )
type Parameter = (Mode, Int)

solve :: IO ()
solve = do
    input <- readFile "input/Day05.txt"
    input
        & parseInput
        & run
        & print . output

step :: Computer -> Computer
step c = performInstruction instruction c
    where
        rawInstruction = getArgument (Position, pointer c) (program c)
        instruction = parseInstruction rawInstruction

run :: Computer -> Computer
run computer@Computer{state=Halted} = computer
run c = run $ step c

getArgument :: Parameter -> Program -> Int
getArgument (Position, x) is = (Map.!) is x
getArgument (Immediate, x) _ = x

setValue :: Int -> Int -> Program -> Program
setValue = Map.insert

parseInput :: String -> Computer
parseInput input = Computer{program=program, state=Running, pointer=0, input=5, output=[]}
    where
        ints = map read $ splitOn "," input
        program = Map.fromList $ zip [0..] ints

parseInstruction :: Int -> Instruction
parseInstruction instruction =
    Instruction { modes=map parseMode modes
                , numberOfArguments=numArgs
                , apply=fn }
    where
        reverseDigits = (reverse . toDigits) instruction
        opcodeDigits = take 2 reverseDigits
        opcode = sum $ map (\(x, y)-> x * (10^y)) (zip opcodeDigits [0..])
        modeDigits = drop 2 reverseDigits
        modes = modeDigits ++ replicate (3 - length modeDigits) 0
        (fn, numArgs) = getOpcodeFunctionAndNumArgs opcode

getOpcodeFunctionAndNumArgs :: Int -> (Opcode, Int)
getOpcodeFunctionAndNumArgs x = case x of
    1 -> (add, 3)
    2 -> (mult, 3)
    3 -> (readInput, 1)
    4 -> (outputArgument, 1)
    5 -> (jumpIfTrue, 2)
    6 -> (jumpIfFalse, 2)
    7 -> (lessThan, 3)
    8 -> (equals, 3)
    99 -> (halt, 1)

parseMode :: Int -> Mode
parseMode 0 = Position
parseMode 1 = Immediate

performInstruction :: Instruction -> Computer -> Computer
performInstruction instruction computer = apply instruction computer params
    where
        numberOfArgs = numberOfArguments instruction
        parameterPointers = map (+ pointer computer) [1..numberOfArgs]
        parameterValues = map (\x -> getArgument (Position, x) (program computer)) parameterPointers
        params = zip (modes instruction) parameterValues

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

advancePointer :: Computer -> Int -> Computer
advancePointer c x = c{pointer=pointer c + x}

add :: Opcode
add c [param1,param2,(_,output)] = advancePointer c{program=newProgram} 4
    where
        a = getArgument param1 (program c)
        b = getArgument param2 (program c)
        newProgram = setValue output (a + b) (program c)

mult :: Opcode
mult c [param1,param2,(_,output)] = advancePointer c{program=newProgram} 4
    where
        a = getArgument param1 (program c)
        b = getArgument param2 (program c)
        newProgram = setValue output (a * b) (program c)

readInput :: Opcode
readInput c [(_,output)] = advancePointer c{program=newProgram} 2
    where
        value = input c
        newProgram = setValue output value (program c)

outputArgument :: Opcode
outputArgument c [param] =
    advancePointer c{output=output c ++ [getArgument param (program c)]} 2

jumpIfTrue :: Opcode
jumpIfTrue c [checkParam, pointerParam] = c{pointer=newPointer}
    where
        valueToCheck = getArgument checkParam (program c)
        newPointer =
            if valueToCheck /= 0
                then getArgument pointerParam (program c)
                else pointer c + 3

jumpIfFalse :: Opcode
jumpIfFalse c [checkParam, pointerParam] = c{pointer=newPointer}
    where
        valueToCheck = getArgument checkParam (program c)
        newPointer =
            if valueToCheck == 0
                then getArgument pointerParam (program c)
                else pointer c + 3

lessThan :: Opcode
lessThan c [param1, param2, (_,output)] = advancePointer c{program=newProgram} 4
    where
        a = getArgument param1 (program c)
        b = getArgument param2 (program c)
        newProgram = setValue output (if a < b then 1 else 0) (program c)

equals :: Opcode
equals c [param1, param2, (_,output)] = advancePointer c{program=newProgram} 4
    where
        a = getArgument param1 (program c)
        b = getArgument param2 (program c)
        newProgram = setValue output (if a == b then 1 else 0) (program c)

halt :: Opcode
halt c _ = c{state=Halted}
