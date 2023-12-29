module AoC2023.Day06

open System.Text.RegularExpressions

let lineParserPart1 line =
    [ for x in Regex("\d+").Matches(line) -> int64 x.Value ]

let lineParserPart2 line =
    [ [ for x in Regex("\d+").Matches(line) -> x.Value ] |> String.concat "" |> int64 ]

let parse lineParser filename =
    let inputs =
        filename |> System.IO.File.ReadAllLines |> Seq.toList |> List.map lineParser

    match inputs with
    | [ times; distances ] -> List.zip times distances
    | _ -> failwith "Invalid input"

let solve races =
    let winningTimes (ms, record) =
        [ 1L .. ms - 1L ]
        |> List.map (fun holdDownMs -> (ms - holdDownMs) * holdDownMs)
        |> List.filter ((<) record)

    races |> List.map winningTimes |> List.map List.length |> List.reduce ((*))

let run filename =
    filename |> parse lineParserPart1 |> solve |> printfn "Part 1: %A"
    filename |> parse lineParserPart2 |> solve |> printfn "Part 2: %A"
