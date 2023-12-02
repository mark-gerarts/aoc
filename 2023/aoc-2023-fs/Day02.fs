module AoC2023.Day02

open FParsec

type Color =
    | Blue
    | Red
    | Green

let parseLine line =
    let pBlue = pstring "blue" >>% Blue
    let pRed = pstring "red" >>% Red
    let pGreen = pstring "green" >>% Green
    let pColor = choice [ pBlue; pRed; pGreen ]
    let pTuple = pipe2 pint32 (spaces >>. pColor) (fun amount color -> amount, color)
    let pSet = sepBy1 pTuple (pstring ", ")
    let pSets = sepBy1 pSet (pstring "; ")
    let pGame = pstring "Game " >>. pint32 .>> pstring ": "
    let pLine = pipe2 pGame pSets (fun game sets -> game, sets)

    match run pLine line with
    | Success(result, _, _) -> result
    | _ -> failwithf "Parse error on line '%s'" line

let isValid game =
    let isValidCount (amount, color) =
        match color with
        | Red when amount <= 12 -> true
        | Green when amount <= 13 -> true
        | Blue when amount <= 14 -> true
        | _ -> false

    game |> List.forall (List.forall isValidCount)

let power game =
    game
    |> List.concat
    |> List.groupBy snd
    |> List.map (snd >> List.map fst >> List.max)
    |> List.reduce (*)

let run filename =
    let games = filename |> System.IO.File.ReadAllLines |> Seq.map parseLine
    games |> Seq.filter (snd >> isValid) |> Seq.sumBy fst |> printfn "Part 1: %i"
    games |> Seq.sumBy (snd >> power) |> printfn "Part 2: %i"
