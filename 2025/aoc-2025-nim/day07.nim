import std/strutils
import std/tables
import std/sequtils
import std/math

let input = readFile("input/07.txt").strip().splitLines()

var
    numSplits = 0
    beamCounts = initCountTable[int](input[0].len())

for line in input:
    for x, symbol in line:
        if symbol == 'S':
            beamCounts[x] = 1
        if symbol == '^' and beamCounts[x] > 0:
            numSplits += 1

            var countAbove: int
            discard beamCounts.pop(x, countAbove)

            beamCounts.inc(x-1, countAbove)
            beamCounts.inc(x+1, countAbove)

echo "Part 1: ", $numSplits
echo "Part 2: ", $beamCounts.values().toSeq().sum()
