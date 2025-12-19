let parse c =
    match c with
    | '(' -> 1
    | ')' -> -1
    | _ -> failwithf "Unexpected character %c" c

let input = System.IO.File.ReadAllText "input/01.txt" |> _.Trim() |> Seq.map parse

input |> Seq.sum |> printfn "Part 1: %i"
input |> Seq.scan (+) 0 |> Seq.findIndex ((=) -1) |> printfn "Part 2: %i"
