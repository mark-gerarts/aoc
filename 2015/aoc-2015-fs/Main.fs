open AoC2015

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputFilename |] ->
        match day with
        | "01" -> Day01.run inputFilename
        | "02" -> Day02.run inputFilename
        | _ -> eprintfn "Day %s does not exist" day
    | _ -> eprintfn "Incorrect arguments. Usage: dotnet run 01 path/to/input.txt"

    0
