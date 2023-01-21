module AoC2022.Day12

open System

type Grid = { grid: array<int>; w: int; h: int }

let rec getHeightForChar char =
    match char with
    | 'S' -> getHeightForChar 'a' - 1
    | 'E' -> getHeightForChar 'z' + 1
    | c -> int c - int 'a'

let parseInput =
    let input = System.IO.File.ReadAllLines("./input/12.txt")
    let w = Seq.length input[0]
    let h = Seq.length input
    let grid = Seq.concat input |> Seq.map getHeightForChar |> Seq.toArray

    { grid = grid; w = w; h = h }

let getCell grid pos =
    match pos with
    | x, _ when x < 0 -> None
    | x, _ when x >= grid.w -> None
    | _, y when y < 0 -> None
    | _, y when y >= grid.h -> None
    | x, y -> Some grid.grid[grid.w * y + x]

let findPositionsForHeight grid height =
    let intToPos i = (i % grid.w, i / grid.w)

    let hasHeight pos =
        match getCell grid pos with
        | None -> false
        | Some h -> h = height

    { 0 .. (grid.w * grid.h) } |> Seq.map intToPos |> Seq.filter hasHeight

let findPositionForHeight grid height =
    findPositionsForHeight grid height |> Seq.head

let neighbouringCells grid (x, y) =
    let height = getCell grid (x, y) |> Option.get

    let isAccessible p =
        match getCell grid p with
        | None -> false
        | Some h -> h <= height + 1

    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] |> List.filter isAccessible

let bfs grid start goal =
    let mutable parents = Map.empty

    let rec reconstruct c =
        match Map.tryFind c parents with
        | Some p -> c :: reconstruct p
        | None -> []

    let rec go queue seen =
        match queue with
        | [] -> None
        | p :: _ when p = goal -> Some <| List.rev (reconstruct p)
        | p :: ps ->
            let unseenNeighbours =
                neighbouringCells grid p |> List.filter (fun p' -> not <| Set.contains p' seen)

            for child in unseenNeighbours do
                parents <- Map.add child p parents

            let newQueue = List.append ps unseenNeighbours
            let newSeen = Set.union seen (Set.ofList unseenNeighbours)

            go newQueue newSeen

    match go (List.singleton start) (Set.singleton start) with
    | None -> Int64.MaxValue
    | Some path -> List.length path

let partA grid =
    let startPos = findPositionForHeight grid (getHeightForChar 'S')
    let endPos = findPositionForHeight grid (getHeightForChar 'E')

    bfs grid startPos endPos

let partB grid =
    let endpos = findPositionForHeight grid (getHeightForChar 'E')
    let possibleStartPositions = findPositionsForHeight grid (getHeightForChar 'a')

    possibleStartPositions |> Seq.map (fun s -> bfs grid s endpos) |> Seq.min

let run =
    let grid = parseInput

    printfn "Part A: %i" (partA grid)
    printfn "Part B: %i" (partB grid)
