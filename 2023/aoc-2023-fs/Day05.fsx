#r "nuget: FParsec,1.1.1"
#r "nuget: FsUnit,6.0.0"
#r "nuget: FSharp.Collections.ParallelSeq,1.2.0"

open FParsec
open FsUnit
open FSharp.Collections.ParallelSeq

type Range =
    { src: int64
      dst: int64
      length: int64 }

let parseInput input =
    let pDigits = sepBy pint64 (pchar ' ')
    let pSeeds = pstring "seeds: " >>. pDigits .>> spaces

    let pMappingHeader = restOfLine true

    let pRange =
        pipe3 (pint64 .>> spaces) (pint64 .>> spaces) pint64 (fun d s l -> { src = s; dst = d; length = l })

    let pMapping = pMappingHeader >>. (sepEndBy pRange newline)
    let pMappings = sepEndBy pMapping newline

    let pAlmanac = pSeeds .>>. pMappings

    match run pAlmanac input with
    | Success(almanac, _, _) -> almanac
    | err -> failwithf "%A" err

let isInRange number range =
    number >= range.src && number < range.src + range.length

let translate number ranges =
    match List.tryFind (isInRange number) ranges with
    | Some range -> range.dst + (number - range.src)
    | None -> number

let (seeds, ranges) = System.IO.File.ReadAllText "input/05.txt" |> parseInput

let processSeed seed =
    List.fold (fun n m -> translate n m) seed ranges

seeds |> List.map processSeed |> List.min |> printfn "Part 1: %i"

// CPU goes brrrr...
let rec part2 seeds =
    seq {
        match seeds with
        | start :: length :: rest ->

            seq { start .. start + length - 1L } |> PSeq.map processSeed |> PSeq.min

            yield! part2 rest
        | [] -> ()
        | _ -> failwithf "Odd amount of seeds as input"
    }

seeds |> part2 |> PSeq.min |> printfn "Part 2: %i"

isInRange 98 { src = 98; dst = 50; length = 2 } |> should equal true
isInRange 99 { src = 98; dst = 50; length = 2 } |> should equal true
isInRange 97 { src = 98; dst = 50; length = 2 } |> should equal false
isInRange 100 { src = 98; dst = 50; length = 2 } |> should equal false

let exampleRanges =
    [ { src = 98; dst = 50; length = 2 }; { src = 50; dst = 52; length = 48 } ]

translate 0 exampleRanges |> should equal 0
translate 49 exampleRanges |> should equal 49
translate 50 exampleRanges |> should equal 52
translate 97 exampleRanges |> should equal 99
translate 98 exampleRanges |> should equal 50
translate 79 exampleRanges |> should equal 81
translate 14 exampleRanges |> should equal 14
translate 55 exampleRanges |> should equal 57
translate 13 exampleRanges |> should equal 13
