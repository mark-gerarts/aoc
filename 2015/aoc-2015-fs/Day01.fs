module AoC2015.Day01

let slurp c =
    match c with
    | '(' -> 1
    | ')' -> -1
    | _ -> failwithf "Unexpected character %c" c

let parse file =
    System.IO.File.ReadAllText(file).Trim() |> Seq.map slurp

let part1 file = file |> parse |> Seq.sum

let part2 file =
    file |> parse |> Seq.scan (+) 0 |> Seq.takeWhile ((<=) 0) |> Seq.length

let run file =
    printfn "Part 1: %i" <| part1 file
    printfn "Part 2: %A" <| part2 file
