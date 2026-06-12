import std/strutils
import std/sugar
import std/sequtils
import std/math

type
    NumberGroup = tuple[numbers: seq[string], operation: (int, int) -> int]

proc parseInput(filename: string): seq[NumberGroup] =
    let lines = readFile(filename).strip().splitLines()
    let numbers = lines[0..^2]
    let operations = lines[^1]

    var groupIndices = toSeq(0..operations.high).filterIt(operations[it] != ' ')
    groupIndices.add(lines[0].len())
    let groupRanges = zip(groupIndices, groupIndices[1..^1])

    collect(newSeq()):
        for (startIndex, endIndex) in groupRanges:
            let operation: (int, int) -> int =
                case operations[startIndex]
                of '*': (x, y: int) => x * y
                of '+': (x, y: int) => x + y
                else: raiseAssert "Unsupported operation"

            let numberLines = collect(newSeq()):
                for numberLine in numbers:
                    $numberLine[startIndex..endIndex-1]

            (numberLines, operation)

proc eval(numberGroup: NumberGroup): int =
    numberGroup.numbers
        .mapIt(it.strip().parseInt())
        .foldl(numberGroup.operation(a, b))

proc fix(numberGroup: NumberGroup): NumberGroup =
    let width = numberGroup.numbers[0].len()

    let fixedNumbers = collect(newSeq()):
        for i in 0..<width:
            var fixedNumber = ""
            for number in numberGroup.numbers:
                if number[i] != ' ':
                    fixedNumber &= number[i]

            if fixedNumber != "": fixedNumber

    (fixedNumbers, numberGroup.operation)


let numberGroups = parseInput("input/06.txt")
echo "Part 1: ", numberGroups.mapIt(it.eval()).sum()
echo "Part 2: ", numberGroups.mapIt(it.fix().eval()).sum()
