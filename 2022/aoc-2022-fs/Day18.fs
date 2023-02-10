module AoC2022.Day18

type Coord = { X: int; Y: int; Z: int }

type Face =
    | Left
    | Right
    | Top
    | Bottom
    | Front
    | Back

type Side = Coord * Face

let parseLine (line: string) =
    let parts = line.Split(',') |> Array.map int

    { X = parts[0]
      Y = parts[1]
      Z = parts[2] }

// When given a side (= coord + face), normalizes to the combination whose
// coordinates are closest to 0. E.g. (2,1,1 Left) becomes (1,1,1 Right)
let normalize (coord, face) =
    match face with
    | Left when coord.X > 0 -> { coord with X = coord.X - 1 }, Right
    | Right when coord.X < 0 -> { coord with X = coord.X + 1 }, Left
    | Top when coord.Y > 0 -> { coord with Y = coord.Y - 1 }, Bottom
    | Bottom when coord.Y < 0 -> { coord with Y = coord.Y + 1 }, Top
    | Front when coord.Z > 0 -> { coord with Z = coord.Z - 1 }, Back
    | Back when coord.Z < 0 -> { coord with Z = coord.Z + 1 }, Front
    | _ -> coord, face

let withAllSides coord =
    [ Left; Right; Top; Bottom; Front; Back ] |> List.map (fun side -> coord, side)

let partA =
    System.IO.File.ReadLines "./input/18.txt"
    |> Seq.map parseLine
    |> Seq.collect withAllSides
    |> Seq.map normalize
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count = 1)
    |> Seq.length

let run = printfn "Part A: %i" partA
