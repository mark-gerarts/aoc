import std/strutils
import std/tables
import std/sequtils
import std/math
import std/strscans
import system/iterators
import std/algorithm
import std/sets
import std/options

type
    Pos = array[3, int]
    Circuit = HashSet[Pos]

proc parseLine(line: string): Pos =
    let (_, x, y, z) = line.scanTuple("$i,$i,$i")
    [x, y, z]

proc distance(a: Pos, b: Pos): float =
    (a[0] - b[0]).toFloat().pow(2.0) +
    (a[1] - b[1]).toFloat().pow(2.0) +
    (a[2] - b[2]).toFloat().pow(2.0)

proc pairs[T](positions: seq[T]): seq[seq[T]] =
    result = @[]

    for i in 0..positions.len()-2:
        for j in i+1..positions.len()-1:
            result.add(@[positions[i], positions[j]])

let input = readFile("input/08.test").strip().splitLines().map(parseLine)
var inputPairs = input.pairs()

inputPairs.sort do (a, b: seq[Pos]) -> int:
    cmp(distance(a[0], a[1]), distance(b[0], b[1]))

var circuits: seq[Circuit] = @[]

proc connect(a, b: Pos) =
    var circuit_a, circuit_b = none(Circuit)

    for i, circuit in circuits:
        if a in circuit:
            circuit_a = some(circuit)

        if b in circuit:
            circuit_b = some(circuit)

    if circuit_a.isNone() and circuit_b.isNone():
        circuits.add(@[a, b].toHashSet())
    elif circuit_a.isNone():
        circuit_b.incl(a)
    elif circuit_b.isNone():
        circuit_a.incl(b)
    else:
        circuit_a circuit_a.union(circuit_b)




    circuits.add(@[a, b].toHashSet())

for pair in inputPairs[0..9]:
    connect(pair[0], pair[1])

echo circuits
