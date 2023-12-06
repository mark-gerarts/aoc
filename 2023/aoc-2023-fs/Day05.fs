module AoC2023.Day05

open System.Text.RegularExpressions
open System
open FSharp.Collections.ParallelSeq

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
    seeds |> PSeq.map (processSeed pipeline) |> PSeq.min

// CPU goes brrrr...
let rec part2 pipeline seeds =
    seq {
        match seeds with
        | [] -> ()
        | _ :: [] -> failwith "Odd number of seeds input"
        | start :: length :: xs ->
            // Collecting all seeds and then PSeq'ing it through findMinLocation
            // ran into memory issues, so let's minimize per batch right here.
            yield
                seq {
                    for x in start .. start + (length - 1L) do
                        yield x
                }
                |> PSeq.map (processSeed pipeline)
                |> PSeq.min

            yield! part2 pipeline xs
    }

let run filename =
    let (seeds, pipeline) = parseInput filename

    findMinLocation pipeline seeds |> printfn "Part 1: %i"
    part2 pipeline seeds |> Seq.min |> printfn "Part 2: %i"
