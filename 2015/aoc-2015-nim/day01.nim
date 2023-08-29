import os
import std/strutils
import std/sequtils

func parseStep(c: char): int =
    case c
    of '(': 1
    of ')': -1
    else: raise newException(Exception, "Invalid character")

let instructions = readFile(paramStr(1))
    .strip()
    .map(parseStep)

var basementIndex = 0
var floor = 0
for i, instruction in instructions.pairs():
    floor += instruction
    if floor == -1 and basementIndex == 0:
        basementIndex = i + 1

echo "Part 1: " & $floor
echo "Part 2: " & $basementIndex
