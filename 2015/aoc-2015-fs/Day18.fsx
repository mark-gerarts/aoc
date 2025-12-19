type Light =
    | On
    | Off

type Grid =
    { w: int
      h: int
      cells: Light array }

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
                    x, y
        }

    member self.neighbours(x, y) =
        [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ] |> List.map self.get

    member self.numOn = self.cells |> Array.filter ((=) On) |> Array.length

    member _.corners = [ 0, 0; 0, 99; 99, 0; 99, 99 ]

let parseInput =
    let cells =
        [| for line in System.IO.File.ReadLines "input/18.txt" do
               for char in line do
                   if char = '#' then On else Off |]

    { w = 100; h = 100; cells = cells }

let stepCell (grid: Grid) stickyCells pos =
    let onNeighbours = grid.neighbours pos |> Seq.filter ((=) On) |> Seq.length

    match grid.get pos with
    | On when onNeighbours = 2 || onNeighbours = 3 -> On
    | Off when onNeighbours = 3 -> On
    | _ when List.contains pos stickyCells -> On
    | _ -> Off


let stepGrid grid stickyCells =
    let newGrid =
        { grid with
            cells = Array.copy grid.cells }

    grid.allCoords
    |> Seq.iter (fun p -> newGrid.set p (stepCell grid stickyCells p))

    newGrid

let stepN n stickyCells grid =
    { 1..n } |> Seq.fold (fun grid _ -> stepGrid grid stickyCells) grid

let grid = parseInput

(stepN 100 [] grid).numOn |> printfn "Part 1: %i"

(stepN 100 grid.corners grid).numOn |> printfn "Part 2: %i"
