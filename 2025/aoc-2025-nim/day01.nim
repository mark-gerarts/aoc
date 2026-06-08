import std/strformat
import std/strutils


func parseLine(line: string): int =
    line.replace("L", "-").replace("R", "").parseInt

func turnDial(dial: int, amount: int): int =
    (dial + amount) mod 100

var
    dial = 50
    numZeroesPart1 = 0
    numZeroesPart2 = 0

for line in lines("input/01.txt"):
    let amount = parseLine(line)
    let step = if amount < 0: -1 else: 1

    for _ in 1..abs(amount):
        dial = turnDial(dial, step)
        if dial == 0: numZeroesPart2 += 1

    if dial == 0: numZeroesPart1 += 1

echo &"Part 1: {numZeroesPart1}"
echo &"Part 2: {numZeroesPart2}"
