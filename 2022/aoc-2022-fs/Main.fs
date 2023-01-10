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
        | _ -> eprintfn "Day %s does not exist" day

    0
