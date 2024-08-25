#r "nuget: FParsec,1.1.1"

open FParsec

type Color =
    | Red
    | Green
    | Blue

let parseLine line =
    let pRed = pstring " red" >>% Red
    let pGreen = pstring " green" >>% Green
    let pBlue = pstring " blue" >>% Blue
    let pColor = choice [ pRed; pGreen; pBlue ]
    let pTuple = pint32 .>>. pColor
    let pSet = sepBy pTuple (pstring ", ")
    let pSets = sepBy pSet (pstring "; ")
    let pGame = pstring "Game " >>. pint32 .>> pstring ": " .>>. pSets

    match run pGame line with
    | Success(game, _, _) -> game
    | _ -> failwithf "Parse error on line %s" line

let isValid game =
    let isValidTuple tuple =
        match tuple with
        | (amount, Red) when amount <= 12 -> true
        | (amount, Green) when amount <= 13 -> true
        | (amount, Blue) when amount <= 14 -> true
        | _ -> false

    game |> snd |> Seq.forall (Seq.forall isValidTuple)

let power game =
    game
    |> snd
    |> Seq.concat
    |> Seq.groupBy snd
    |> Seq.map (snd >> Seq.maxBy fst)
    |> Seq.map fst
    |> Seq.reduce (*)

let games = System.IO.File.ReadLines "input/02.txt" |> Seq.map parseLine
games |> Seq.filter isValid |> Seq.sumBy fst |> printfn "Part 1: %i"
games |> Seq.sumBy power |> printfn "Part 2: %i"
