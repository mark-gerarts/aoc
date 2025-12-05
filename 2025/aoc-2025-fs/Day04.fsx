let neighbours (x, y) =
    [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]
    |> List.map (fun (dx, dy) -> x + dx, y + dy)

let grid =
    seq {
        for y, line in Seq.indexed (System.IO.File.ReadLines "input/04.txt") do
            for x, char in Seq.indexed line do
                if char = '@' then
                    yield x, y
    }
    |> Set.ofSeq

let removeAccessiblePapers grid =
    let isAccessible p =
        neighbours p |> Seq.filter (fun p -> Set.contains p grid) |> Seq.length |> (>) 4

    grid |> Set.filter (not << isAccessible)

let rec removeUntilStabilized grid =
    match removeAccessiblePapers grid with
    | newGrid when Set.count newGrid = Set.count grid -> grid
    | newGrid -> removeUntilStabilized newGrid

let startingPaperCount = Set.count grid
let countAfterOneStep = removeAccessiblePapers grid |> Set.count
let stabilizedCount = removeUntilStabilized grid |> Set.count

printfn "Part 1: %i" (startingPaperCount - countAfterOneStep)
printfn "Part 2: %i" (startingPaperCount - stabilizedCount)
