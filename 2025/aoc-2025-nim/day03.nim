import std/sequtils
import std/strutils
import std/math


proc maxJoltage(bank: string, numBatteries: int): int =
    let bank = bank.mapIt(parseInt($it))
    var currentIndex = 0

    for n in countdown(numBatteries, 1):
        let highestIndex = bank[currentIndex..^n].maxIndex()
        result += bank[currentIndex + highestIndex] * 10^(n-1)
        currentIndex += highestIndex + 1

let input = "input/03.txt".lines().toSeq()
echo "Part 1: " & $input.mapIt(maxJoltage(it, 2)).sum()
echo "Part 2: " & $input.mapIt(maxJoltage(it, 12)).sum()
