import std/algorithm
import std/strutils
import std/sequtils
import std/math
import os

let topThree = readFile(paramStr(1))
  .strip()
  .split("\n\n")
  .mapIt(
    it
    .split("\n")
    .map(parseInt)
    .sum())
  .sorted(Descending)[0..2]

echo "Part 1: " & $topThree[0]
echo "Part 2: " & $topThree.sum
