open AoC2015

[<EntryPoint>]
let main args =
    match args with
    | [| day; inputFilename |] ->
        let dayFn =
            match day with
            | "01"
            | "1" -> Day01.run
            | "02"
            | "2" -> Day02.run
            | "03"
            | "3" -> Day03.run
            | "04"
            | "4" -> Day04.run
            | "05"
            | "5" -> Day05.run
            | "06"
            | "6" -> Day06.run
            | _ -> failwithf "Day %s does not exist" day

        dayFn inputFilename
    | _ -> failwith "Incorrect arguments. Usage: dotnet run 01 path/to/input.txt"

    0
