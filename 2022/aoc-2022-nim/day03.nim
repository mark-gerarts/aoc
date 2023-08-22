import os
import std/strutils
import std/sets
import std/sequtils
import std/math
import sugar

func getScore(char: char): int =
    if char.isUpperAscii():
        int(char) - int('A') + 26 + 1
    else:
        int(char) - int('a') + 1

proc parseInput(): seq[string] =
    readFile(paramStr(1))
        .strip()
        .split("\n")

proc solvePart1(): int =
    func getScoreForLine(line: string): int =
        let
            half = line.len div 2
            left = line[0..half - 1].toHashSet()
            right = line[half..^1].toHashSet()

        (left * right).map(getScore).foldl(a + b, 0)

    parseInput()
        .map(getScoreForLine)
        .sum()

proc solvePart2(): int =
    let input = parseInput()
    let groups = input.distribute(input.len div 3)

    groups
        .mapIt(
            it.map(s => s.toHashSet())
            .foldl(a * b)
            .map(getScore)
            .foldl(a + b, 0))
        .sum()

echo "Part 1: " & $solvePart1()
echo "Part 2: " & $solvePart2()
