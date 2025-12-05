type Pos = int * int

type Grid = Set<Pos>

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


let countPapers = Set.count

let removeAccessiblePapers grid =
    let isAccessible p =
        neighbours p |> Seq.filter (fun p -> Set.contains p grid) |> Seq.length |> (>) 4

    grid |> Set.filter (not << isAccessible)

let rec keepRemoving grid =
    seq {
        yield grid
        yield! keepRemoving <| removeAccessiblePapers grid
    }

let paperCounts = keepRemoving grid |> Seq.map countPapers

let stabilizedPaperCounts =
    Seq.zip paperCounts (Seq.tail paperCounts)
    |> Seq.takeWhile (fun (a, b) -> a <> b)
    |> Seq.collect (fun (a, b) -> [ a; b ])
    |> Seq.distinct

let startingPaperCount = Seq.head stabilizedPaperCounts
let countAfterOneStep = Seq.tail stabilizedPaperCounts |> Seq.head
let stabilizedCount = Seq.last stabilizedPaperCounts

printfn "Part 1: %i" (startingPaperCount - countAfterOneStep)
printfn "Part 2: %i" (startingPaperCount - stabilizedCount)
