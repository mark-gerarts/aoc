﻿open AoC2016

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
            | "07"
            | "7" -> Day07.run
            | "08"
            | "8" -> Day08.run
            | "09"
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
            | "19" -> Day19.run
            | "20" -> Day20.run
            | "21" -> Day21.run
            | "22" -> Day22.run
            | "23" -> Day23.run
            | "24" -> Day24.run
            | "25" -> Day25.run
            | _ -> failwithf "Day %s does not exist" day

        dayFn inputFilename
    | _ -> failwith "Incorrect arguments. Usage: dotnet run 01 path/to/input.txt"

    0
