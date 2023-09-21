module AoC2015.Day01

let parse c =
    match c with
    | '(' -> 1
    | ')' -> -1
    | _ -> failwithf "Unexpected character %c" c

let run filename =
    let input = System.IO.File.ReadAllText(filename).Trim() |> Seq.map parse

    input |> Seq.sum |> printfn "Part 1: %i"

    input
    |> Seq.scan (+) 0
    |> Seq.takeWhile ((<=) 0)
    |> Seq.length
    |> printfn "Part 2: %i"
