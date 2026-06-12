import std/strutils
import std/strscans
import std/sequtils
import std/sugar
import std/algorithm
import std/math

type
    Range = tuple[left: int, right: int]

proc parseInput(filename: string): (seq[Range], seq[int]) =
    let inputParts = readFile(filename).strip().split("\n\n")
    let (rangeInput, idInput) = (inputParts[0], inputParts[1])

    let ids = idInput.split('\n').map(parseInt)
    let ranges = rangeInput.splitLines().map do (range: string) -> Range:
        let (_, left, right) = range.scanTuple("$i-$i")
        (left, right)

    (ranges, ids)

proc `in`(id: int, range: Range): bool =
    id >= range.left and id <= range.right

proc mergeAll(ranges: openArray[Range]): seq[Range] =
    var sortedRanges = ranges.toSeq()
    sortedRanges.sort()

    result = sortedRanges[0..<1]
    for (left, right) in sortedRanges:
        let (_, currentMax) = result[^1]
        if left > currentMax:
            result.add((left, right))
        else:
            result[^1].right = max(result[^1].right, right)


let (ranges, ids) = parseInput("input/05.txt")
echo "Part 1: ", ids.filter(id => ranges.anyIt(id in it)).len()
echo "Part 2: ", ranges.mergeAll().mapIt(it.right - it.left + 1).sum()
