import os
import std/strutils
import std/re
import std/sequtils
import std/algorithm
import sugar
import std/strscans

type
    Instruction = object
        amount: int
        src: int
        dst: int

    Stack = seq[char]

    Stacks = seq[Stack]

func parseInitialConfig(input: string): Stacks =
    let lines = input.split("\n")
    let numStacks = lines[^1].len div 4 + 1

    var stacks = newSeq[Stack](numStacks)

    for line in input.split("\n")[0..^2]:
        for i, c in line.pairs():
            if c in "[] ": continue

            let stackNumber = i div 4
            stacks[stackNumber].add(c)

    stacks.map(s => s.reversed)

func parseInstructions(input: string): seq[Instruction] =
    func parseInstruction(line: string): Instruction =
        let (_, amount, src, dst) = line.scanTuple("move $i from $i to $i")
        Instruction(amount: amount, src: src, dst: dst)

    input.strip().split("\n").map(parseInstruction)

func readMessage(stacks: Stacks): string =
    stacks.map(s => s[^1]).join()

proc applyOneAtATime(stacks: var Stacks, instruction: Instruction) =
    for _ in 0 ..< instruction.amount:
        let c = stacks[instruction.src - 1].pop()
        stacks[instruction.dst - 1].add(c)

proc applyAllAtOnce(stacks: var Stacks, instruction: Instruction) =
    var
        amount = instruction.amount
        src = stacks[instruction.src - 1]
        slice = src[src.len - amount .. src.high]

    stacks[instruction.dst - 1].add(slice)
    stacks[instruction.src - 1] = src[0 .. src.high - amount]

proc solve(applyFn: proc): string =
    let
        inputParts = readFile(paramStr(1)).split("\n\n")
        instructions = parseInstructions(inputParts[1])

    var stacks = parseInitialConfig(inputParts[0])

    for instruction in instructions:
        applyFn(stacks, instruction)

    stacks.readMessage()

echo "Part 1: " & solve(applyOneAtATime)
echo "Part 2: " & solve(applyAllAtOnce)
