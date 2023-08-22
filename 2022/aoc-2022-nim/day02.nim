import std/strformat
import std/sequtils
import std/strutils
import std/math
import os

type
    Element = enum
        rock
        paper
        scissors

    Outcome = enum
        win
        loss
        draw

    Strategy = enum
        pickElement
        pickOutcome

func parseElement(char: char): Element =
    case char
    of 'A', 'X': rock
    of 'B', 'Y': paper
    of 'C', 'Z': scissors
    else: raise newException(Exception, fmt"Unexpected char '{char}'")

func parseOutcome(char: char): Outcome =
    case char
    of 'X': loss
    of 'Y': draw
    of 'Z': win
    else: raise newException(Exception, fmt"Unexpected char '{char}'")

func beats(element: Element, other: Element): bool =
    case element
    of rock: other == scissors
    of paper: other == rock
    of scissors: other == paper

func fight(a: Element, b: Element): Outcome =
    if a.beats(b): win
    elif b.beats(a): loss
    else: draw

func getElementForDesiredOutcome(el: Element, outcome: Outcome): Element =
    let elements = [rock, paper, scissors]

    case outcome
    of draw: el
    of win: elements.filterIt(it.beats(el))[0]
    of loss: elements.filterIt(el.beats(it))[0]

func getScoreForLine(strategy: Strategy, line: string): int =
    let theirElement = parseElement(line[0])

    let ourElement =
        case strategy
        of pickElement: parseElement(line[2])
        of pickOutcome:
            let outcome = parseOutcome(line[2])
            theirElement.getElementForDesiredOutcome(outcome)

    let outcomeScore =
        case ourElement.fight(theirElement)
        of win: 6
        of loss: 0
        of draw: 3

    let elementScore =
        case ourElement
        of rock: 1
        of paper: 2
        of scissors: 3

    outcomeScore + elementScore

proc solve(strategy: Strategy): int =
    readFile(paramStr(1))
        .strip()
        .split("\n")
        .mapIt(getScoreForLine(strategy, it))
        .sum()

echo "Part 1: " & $solve(pickElement)
echo "Part 2: " & $solve(pickOutcome)
