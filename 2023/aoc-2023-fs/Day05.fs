module AoC2023.Day05

open System.Text.RegularExpressions
open System

type RangeMap =
    { srcStart: Int64
      dstStart: Int64
      length: Int64 }

let mapSingle src rangeMap =
    if src >= rangeMap.srcStart && src < rangeMap.srcStart + rangeMap.length then
        Some <| rangeMap.dstStart + (src - rangeMap.srcStart)
    else
        None

let map src rangeMaps =

    match rangeMaps |> List.choose (mapSingle src) with
    | [ dst ] -> dst
    | [] -> src
    | _ -> failwith "Should not be possible"

let parseInput filename =
    let digits line =
        [ for m in Regex.Matches(line, "\d+") -> System.Int64.Parse m.Value ]

    let sectionToRangeMaps (section: string) =
        section.Split('\n')
        |> List.ofArray
        |> List.tail
        |> List.map digits
        |> List.map (fun digits ->
            { srcStart = digits[1]
              dstStart = digits[0]
              length = digits[2] })

    let input = filename |> System.IO.File.ReadAllText |> (fun s -> s.Trim())
    let sections = input.Split("\n\n") |> List.ofArray
    let seeds = sections |> List.head |> digits
    let pipeline = sections |> List.tail |> List.map sectionToRangeMaps

    (seeds, pipeline)

let processSeed pipeline seed = pipeline |> List.fold map seed

let findMinLocation pipeline seeds =
    seeds |> List.map (processSeed pipeline) |> List.min

let run filename =
    let (seeds, pipeline) = parseInput filename

    findMinLocation pipeline seeds |> printfn "Part 1: %i"
