import os
import std/strutils
import std/strscans
import std/sequtils
import std/math
import std/algorithm

type Dimensions = (int, int, int)

func parseLine(line: string): Dimensions =
    let (_, l, w, h) = scanTuple(line, "$ix$ix$i")

    (l, w, h)

func paperNeeded(dims: Dimensions): int =
    let
        (l, w, h) = dims
        lw = l * w
        wh = w * h
        hl = h * l
        smallest = [lw, wh, hl].min()

    2*lw + 2*wh + 2*hl + smallest

func ribbonNeeded(dims: Dimensions): int =
    let
        (l, w, h) = dims
        volume = l * w * h
        smallestTwo = [l, w, h].sorted()[0..1]
        perimeter = 2 * smallestTwo[0] + 2 * smallestTwo[1]

    perimeter + volume

let input = readFile(paramStr(1))
    .strip()
    .split("\n")
    .map(parseLine)

echo "Part 1: " & $input.map(paperNeeded).sum()
echo "Part 2: " & $input.map(ribbonNeeded).sum()
