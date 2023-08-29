open AoC2015

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputFilename |] ->
        let dayFn =
            match day with
            | "01" -> Day01.run
            | "02" -> Day02.run
            | "03" -> Day03.run
            | "04" -> Day04.run
            | "05" -> Day05.run
            | _ -> failwithf "Day %s does not exist" day

        dayFn inputFilename
    | _ -> failwith "Incorrect arguments. Usage: dotnet run 01 path/to/input.txt"

    0
