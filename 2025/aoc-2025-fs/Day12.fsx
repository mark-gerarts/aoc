type Grid = { w: int; h: int; grid: bool array }

// The packing problem is NP-hard. `packShapes` uses a proper packing algorithm
// (with a terrible runtime), `fitsShapes` is the lazy gimmicky solution.

let emptyGrid (w, h) =
    { w = w
      h = h
      grid = Array.replicate (w * h) false }

let posToIndex grid (x, y) = grid.w * y + x

let indexToPos grid i = i % grid.w, i / grid.w

let allPositions grid =
    List.init (Array.length grid.grid) (indexToPos grid)

let rotate a =
    let rotateMapping index =
        match index with
        | 0 -> 2
        | 1 -> 5
        | 2 -> 8
        | 3 -> 1
        | 4 -> 4
        | 5 -> 7
        | 6 -> 0
        | 7 -> 3
        | 8 -> 6
        | _ -> failwith "No 3x3 shape"

    Array.permute rotateMapping a

let flipH a =
    let flipMapping index =
        match index with
        | 0 -> 2
        | 1 -> 1
        | 2 -> 0
        | 3 -> 5
        | 4 -> 4
        | 5 -> 3
        | 6 -> 8
        | 7 -> 7
        | 8 -> 6
        | _ -> failwith "No 3x3 shape"

    Array.permute flipMapping a

let flipV a =
    let flipMapping index =
        match index with
        | 0 -> 6
        | 1 -> 7
        | 2 -> 8
        | 3 -> 3
        | 4 -> 4
        | 5 -> 5
        | 6 -> 0
        | 7 -> 1
        | 8 -> 2
        | _ -> failwith "No 3x3 shape"

    Array.permute flipMapping a

let allImages shape =
    let rotations shape =
        Seq.scan (fun s _ -> rotate s) shape { 0..3 }

    seq {
        yield! rotations shape
        yield! rotations (flipH shape)
        yield! rotations (flipV shape)
    }
    |> Seq.distinct
    |> Seq.toList

let isFree grid (x, y) =
    if x >= grid.w || x < 0 || y >= grid.h || y < 0 then
        false
    else
        match Array.tryItem (posToIndex grid (x, y)) grid.grid with
        | Some v -> not v
        | None -> false

let tryInsert grid (x, y) piece =
    let piecePositions =
        seq {
            for dy in { 0..2 } do
                for dx in { 0..2 } do
                    yield x + dx, y + dy
        }
        |> Seq.indexed
        |> Seq.filter (fun (i, _) -> Array.item i piece)
        |> Seq.map snd

    if Seq.forall (isFree grid) piecePositions then
        let newGrid =
            { grid with
                grid = Array.copy grid.grid }

        for p in piecePositions do
            newGrid.grid[posToIndex grid p] <- true

        Some newGrid
    else
        None

let packShapes (shapes: (bool array) array) packingProblem =
    let size, shapeCounts = packingProblem

    let shapes =
        shapeCounts
        |> Array.mapi (fun i count -> Array.replicate count shapes[i])
        |> Array.collect id
        |> Array.toList

    let rec go grid pieces positions =
        match pieces with
        | [] -> true
        | [] :: _ -> false
        | (image :: images) :: otherPieces ->
            match positions with
            | [] -> go grid (images :: otherPieces) (allPositions grid)
            | p :: ps ->
                match tryInsert grid p image with
                | Some newGrid when go newGrid otherPieces (allPositions newGrid) -> true
                | _ -> go grid pieces ps

    let grid = emptyGrid size

    go grid (List.map allImages shapes) (allPositions grid)

let parseInput =
    let parseShapeSection (section: string) =
        section.Split "\n"
        |> Array.tail
        |> System.String.Concat
        |> Seq.map ((=) '#')
        |> Seq.toArray

    let parsePackingProblem (line: string) =
        match line.Split ": " with
        | [| size; amounts |] ->
            match size.Split "x" with
            | [| w; h |] -> (int w, int h), amounts.Split " " |> Array.map int
            | _ -> failwithf "Unable to parse wxh %s" size
        | _ -> failwithf "Unable to parse line %s" line

    let input = System.IO.File.ReadAllText "input/12.txt" |> _.Trim()
    let sections = input.Split "\n\n"
    let shapeSections = sections[0 .. sections.Length - 2]
    let packingProblems = Array.last sections

    shapeSections |> Array.map parseShapeSection, packingProblems.Split "\n" |> Array.map parsePackingProblem

let _, packingProblems = parseInput

let fitsShapes packingProblem =
    let (w, h), shapeCounts = packingProblem
    let maximumSpaceNeeded = Array.sum shapeCounts * (3 * 3)
    let availableSpace = w * h

    availableSpace >= maximumSpaceNeeded

packingProblems |> Seq.filter fitsShapes |> Seq.length |> printfn "Part 1: %i"
