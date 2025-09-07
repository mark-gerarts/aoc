type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let parseGrid input =
    seq {
        for y, line in Seq.indexed input do
            for x, char in Seq.indexed line do
                yield (x, y), char
    }
    |> Map.ofSeq


let move (x, y) direction =
    match direction with
    | N -> x, y + 1
    | NE -> x + 1, y + 1
    | E -> x + 1, y
    | SE -> x + 1, y - 1
    | S -> x, y - 1
    | SW -> x - 1, y - 1
    | W -> x - 1, y
    | NW -> x - 1, y + 1


let xmasMatches grid pos =
    let rec go pos (stringToFind: string) direction =
        match stringToFind, Map.tryFind pos grid with
        | "", _ -> true
        | stringToFind, Some c when c = stringToFind[0] ->
            let nextPos = move pos direction
            go nextPos stringToFind[1..] direction
        | _ -> false

    [ N; NE; E; SE; S; SW; W; NW ] |> List.filter (go pos "XMAS")


let x_masMatches grid pos = failwith "TODO"

let part1 grid =
    grid |> Map.keys |> Seq.collect (xmasMatches grid) |> Seq.length

let grid = System.IO.File.ReadLines "input/04.test" |> parseGrid
grid |> part1 |> printfn "Part 1: %i"
