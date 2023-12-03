module AoC2023.Day03

open System.Text.RegularExpressions

type Grid =
    { cells: char array
      w: int
      h: int }

    member self.idxToPos idx = (idx % self.w, idx / self.w)

    member _.neighbours(x, y) =
        seq {
            for incX in [ -1; 0; 1 ] do
                for incY in [ -1; 0; 1 ] do
                    if incX <> 0 || incY <> 0 then
                        yield (x + incX, y + incY)
        }
        |> Seq.toList

    override self.ToString() = System.String(self.cells)

let parseGrid filename =
    let lines = filename |> System.IO.File.ReadAllLines
    let w = lines |> Seq.head |> Seq.length
    let h = lines |> Seq.length
    let cells = lines |> Seq.concat |> Seq.toArray

    { cells = cells; w = w; h = h }

let numberPositions (grid: Grid) =
    let toCoords idx length =
        List.init length id |> List.map ((+) idx >> grid.idxToPos)

    seq {
        for m in Regex.Matches(grid.ToString(), "\d+") do
            let idx = m.Index
            let length = m.Length
            let value = m.Value

            let (_, y1) = grid.idxToPos idx
            let (_, y2) = grid.idxToPos (idx + length - 1)

            if y1 = y2 then
                yield (int value, toCoords idx length)
            else
                // Account for lines that end with a number, while the next line
                // starts with one.
                let l' = grid.w - (idx % grid.w)

                yield (value.Substring(0, l') |> int, toCoords idx l')
                yield (value.Substring(l') |> int, toCoords (idx + l') (length - l'))
    }
    |> Seq.toList

let symbolPositions (grid: Grid) =
    let matches = Regex.Matches(grid.ToString(), "[^0-9\.]{1}")
    [ for m in matches -> grid.idxToPos m.Index ]

let intersects l1 l2 =
    l1 |> List.exists (fun x -> List.contains x l2)

let gearPositions (grid: Grid) =
    let matches = Regex.Matches(grid.ToString(), "\*")
    [ for m in matches -> grid.idxToPos m.Index ]

let part1 grid =
    let numberPositions = numberPositions grid
    let validPositions = symbolPositions grid |> List.map grid.neighbours |> List.concat

    numberPositions
    |> List.filter (snd >> intersects validPositions)
    |> List.sumBy fst

let part2 grid =
    let numberPositions = numberPositions grid

    let adjacentNumbers pos =
        let neighbours = grid.neighbours pos
        numberPositions |> List.filter (snd >> intersects neighbours) |> List.map fst

    gearPositions grid
    |> List.map adjacentNumbers
    |> List.filter (List.length >> ((=) 2))
    |> List.sumBy (List.reduce (*))


let run filename =
    let grid = parseGrid filename

    grid |> part1 |> printfn "Part 1: %i"
    grid |> part2 |> printfn "Part 2: %i"
