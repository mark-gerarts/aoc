module AoC2022.Day03

let priority char =
    let asciiValue = int char

    if asciiValue < 97 then asciiValue - 38 else asciiValue - 96

let rucksackScore bag =
    let middle = String.length bag / 2

    let left = bag[.. middle - 1]
    let right = bag[middle..]

    let uniqueChar = Set.intersect (Set.ofSeq left) (Set.ofSeq right)

    Set.toSeq uniqueChar |> Seq.head |> priority

let solveA =
    System.IO.File.ReadAllLines("./input/03.txt") |> Seq.sumBy rucksackScore

let solveB =
    let badgePriority group =
        group |> Set.intersectMany |> Set.toSeq |> Seq.head |> priority

    System.IO.File.ReadAllLines("./input/03.txt")
    |> Seq.map Set.ofSeq
    |> Seq.chunkBySize 3
    |> Seq.sumBy badgePriority

let run =
    printfn "Part A: %i" solveA
    printfn "Part B: %i" solveB
