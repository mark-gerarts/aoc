module AoC2022.Day14

open System
open System.Text.RegularExpressions

type Pos = int * int

type Cell =
    | Wall
    | Sand
    | Air

type Grid = Map<Pos, Cell>

type SandMovementResult =
    | Fall of Grid
    | Rest of Grid

let width grid =
    grid |> Map.keys |> Seq.map fst |> Seq.max

let height grid =
    grid |> Map.keys |> Seq.map snd |> Seq.max

let line (x1, y1) (x2, y2) =
    if x1 = x2 && y1 = y2 then
        failwith "Trying to draw a line between identical points"
    else if x1 = x2 then
        let min = if y2 > y1 then y1 else y2
        let max = if y2 > y1 then y2 else y1

        { 0 .. max - min } |> Seq.map (fun i -> (x1, min + i))
    else if y1 = y2 then
        let min = if x2 > x1 then x1 else x2
        let max = if x2 > x1 then x2 else x1

        { 0 .. max - min } |> Seq.map (fun i -> (min + i, y1))
    else
        failwith "No diagonal lines allowed"

let parseLine lineInput =
    let parsePos (pos: string) =
        let parts = pos.Split(',')

        if parts.Length <> 2 then
            failwith $"Invalid position: {pos}"

        int parts[0], int parts[1]

    let positions = Regex(" -> ").Split lineInput |> Seq.map parsePos

    Seq.zip positions (Seq.tail positions)
    |> Seq.collect (fun (p1, p2) -> line p1 p2)

let parseInput =
    System.IO.File.ReadLines("./input/14.txt")
    |> Seq.collect parseLine
    |> Seq.fold (fun m p -> Map.add p Wall m) Map.empty

let rec dropSand (x, y) grid =
    let downLeft = (x - 1, y + 1)
    let down = (x, y + 1)
    let downRight = (x + 1, y + 1)

    let empty p =
        match Map.tryFind p grid with
        | Some Air -> true
        | None -> true
        | _ -> false

    let isOutOfBounds (_, y) = y > height grid

    match empty downLeft, empty down, empty downRight with
    | _, true, _ when isOutOfBounds down -> Fall grid
    | true, _, _ when isOutOfBounds downLeft -> Fall grid
    | _, _, true when isOutOfBounds downRight -> Fall grid
    | _, true, _ -> dropSand down grid
    | true, _, _ -> dropSand downLeft grid
    | _, _, true -> dropSand downRight grid
    | false, false, false -> Rest <| Map.add (x, y) Sand grid

let rec dropSandUntilFull grid =
    match dropSand (500, 0) grid with
    | Fall _ -> grid
    | Rest g -> dropSandUntilFull g

let countSand grid =
    Map.values grid |> Seq.filter (fun x -> x = Sand) |> Seq.length

let printGrid grid =
    let getCharForPos p =
        match Map.tryFind p grid with
        | Some Wall -> '#'
        | Some Sand -> 'o'
        | Some Air -> '.'
        | None -> '.'

    let w = width grid
    let h = height grid

    for y in { 0..h } do
        let line = { 450..w } |> Seq.map (fun x -> getCharForPos (x, y)) |> String.Concat
        printfn "%s" line


let run =
    let grid = parseInput
    let fullGrid = dropSandUntilFull grid

    printGrid fullGrid

    printfn "Part A: %i" <| countSand fullGrid
