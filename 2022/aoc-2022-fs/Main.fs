open AoC2022

[<EntryPoint>]
let main args =
    match Seq.tryHead (args) with
    | None -> eprintfn "Pass the day to run as the first argument, e.g. `dotnet run 1`."
    | Some day ->
        match day with
        | "1" -> Day01.run
        | "2" -> Day02.run
        | "3" -> Day03.run
        | "4" -> Day04.run
        | "5" -> Day05.run
        | "6" -> Day06.run
        | "7" -> Day07.run
        | "8" -> Day08.run
        | "9" -> Day09.run
        | "10" -> Day10.run
        | "11" -> Day11.run
        | "12" -> Day12.run
        | "13" -> Day13.run
        | "14" -> Day14.run
        | "15" -> Day15.run
        | "16" -> Day16.run
        | "17" -> Day17.run
        | "18" -> Day18.run
        | _ -> eprintfn "Day %s does not exist" day

    0
