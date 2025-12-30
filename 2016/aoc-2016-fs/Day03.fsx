open System.Text.RegularExpressions

let parseLine (line: string) =
    Regex("\s+").Split(line.Trim()) |> Seq.map int |> Seq.toList

let isValidTriangle sides =
    match List.sort sides with
    | [ a; b; c ] -> a + b > c
    | _ -> false

let countValidTriangles triangles =
    triangles |> Seq.filter isValidTriangle |> Seq.length

let input = System.IO.File.ReadAllLines "input/03.txt" |> Seq.map parseLine

input |> countValidTriangles |> printfn "Part 1: %i"

input
|> Seq.transpose
|> Seq.collect id
|> Seq.splitInto (Seq.length input)
|> Seq.map Seq.toList
|> countValidTriangles
|> printfn "Part 2: %i"
