import os
import std/strutils
import std/sequtils
import std/strscans
import sugar

type
    Range = object
        start: int
        `end`: int

func parseLine(line: string): (Range, Range) =
    let (_, s1, e1, s2, e2) = line.scanTuple("$i-$i,$i-$i")

    (Range(start: s1, `end`: e1), Range(start: s2, `end`: e2))

func contains(range: Range, other: Range): bool =
    range.start <= other.start and range.end >= other.end

func overlaps(range: Range, other: Range): bool =
    (range.start <= other.start and range.end >= other.start) or
    (range.start <= other.end and range.end >= other.end)

proc solve(filterFn: (Range, Range) -> bool): int =
    readFile(paramStr(1))
        .strip()
        .split("\n")
        .map(parseLine)
        .filter(r => filterFn(r[0], r[1]) or filterFn(r[1], r[0]))
        .len()

echo "Part 1: " & $solve(contains)
echo "Part 2: " & $solve(overlaps)
