import os
import std/sequtils

func findUniqueWindow(input: string, windowSize: int): int =
    for i in 0..input.high:
        let slice = input[i ..< i+windowSize]

        if slice.len == slice.deduplicate().len:
            return i + windowSize

let input = readFile(paramStr(1))

echo "Part 1: " & $findUniqueWindow(input, 4)
echo "Part 2: " & $findUniqueWindow(input, 14)
