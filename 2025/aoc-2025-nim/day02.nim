import std/strutils
import std/sequtils
import std/strformat


type Validator = proc(number: int): bool

func parseRange(range: string): (int, int) =
    let parts = range.split('-')
    (parts[0].parseInt, parts[1].parseInt)

func isInvalidPart1(number: int): bool =
    let numberAsStr = number.intToStr()
    let half = numberAsStr.len div 2

    numberAsStr[0..<half] == numberAsStr[half..^1]

func isInvalidPart2(number: int): bool =
    let numberAsStr = number.intToStr()
    let half = numberAsStr.len div 2

    for n in countdown(half, 1, 1):
        let part = numberAsStr[0..<n]
        if part.repeat(numberAsStr.len div n) == numberAsStr:
            return true

    return false

proc solve(isInvalid: Validator): int =
    let ranges = readFile("input/02.txt").strip().split(',').mapIt(parseRange(it))

    for (left, right) in ranges:
        for number in left..right:
            if isInvalid(number): result += number

echo &"Part 1: {solve(isInvalidPart1)}"
echo &"Part 2: {solve(isInvalidPart2)}"
