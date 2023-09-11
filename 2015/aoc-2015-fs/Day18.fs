module AoC2015.Day18

type CellState =
    | On
    | Off

type Grid =
    { w: int
      h: int
      cells: CellState array }

    member this.get(x, y) =
        if x < 0 || x >= this.w || y < 0 || y >= this.h then
            Off
        else
            this.cells[y * this.w + x]

    member this.set (x, y) state = this.cells[y * this.w + x] <- state

    member this.allCoords =
        seq {
            for x in { 0 .. this.w - 1 } do
                for y in { 0 .. this.h - 1 } do
                    yield (x, y)
        }

    member this.neighbours(x, y) =
        let xIncs = [ -1; 0; 1 ]
        let yIncs = [ -1; 0; 1 ]

        List.allPairs xIncs yIncs
        |> Seq.map (fun (x', y') -> x' + x, y + y')
        |> Seq.filter (fun (x', y') -> not (x = x' && y = y'))
        |> Seq.map this.get

    member this.numOn = this.cells |> Array.filter ((=) On) |> Array.length

    member this.corners = [ (0, 0); (0, 99); (99, 0); (99, 99) ]

let parseChar c =
    match c with
    | '#' -> On
    | '.' -> Off
    | _ -> failwithf "Unexpected character '%c'" c

let parseGrid (input: string) =
    let lines = input.Trim().Split('\n')
    let w = String.length lines[0]
    let h = Array.length lines

    let cells =
        Array.init (w * h) (fun i ->
            let x = i % w
            let y = i / w
            parseChar (lines[y][x]))

    { w = w; h = h; cells = cells }

let stepCell (grid: Grid) pos =
    let onNeighbours = grid.neighbours pos |> Seq.filter ((=) On) |> Seq.length

    match grid.get pos with
    | On when onNeighbours = 2 || onNeighbours = 3 -> On
    | Off when onNeighbours = 3 -> On
    | _ -> Off

let stepGrid grid sticky =
    let newGrid =
        { grid with
            cells = Array.copy grid.cells }

    let stepCellSticky (grid: Grid) pos =
        let isCorner = grid.corners |> List.contains pos

        match isCorner, stepCell grid pos with
        | true, _ -> On
        | false, newState -> newState

    let step = if sticky then stepCellSticky else stepCell

    grid.allCoords |> Seq.iter (fun p -> newGrid.set p (step grid p))

    newGrid

let rec stepN grid n sticky =
    match n with
    | 0 -> grid
    | n -> stepN (stepGrid grid sticky) (n - 1) sticky

let run filename =
    let grid = filename |> System.IO.File.ReadAllText |> parseGrid

    (stepN grid 100 false).numOn |> printfn "Part 1: %i"

    // Ensure corners are turned on.
    grid.corners |> List.iter (fun p -> grid.set p On)
    (stepN grid 100 true).numOn |> printfn "Part 2: %i"
