#r "nuget: FParsec,1.1.1"

open FParsec

type Sequence =
    | Normal of string
    | Hypernet of string

let value sequence =
    match sequence with
    | Normal s -> s
    | Hypernet s -> s

let parse line =
    let pAlpha = regex "[a-z]+"
    let pHypernet = pchar '[' >>. pAlpha .>> pchar ']' |>> Hypernet
    let pNormal = pAlpha |>> Normal

    let pIPv7 = many (pHypernet <|> pNormal)

    match run pIPv7 line with
    | Success(out, _, _) -> out
    | _ -> failwithf "Unable to parse IPv7 %s" line

let containsABBA s =
    s |> Seq.windowed 4 |> Seq.exists (fun s -> s = Array.rev s && s[0] <> s[1])

let supportsTLS ipv7 =
    let parts = parse ipv7

    let abbaInNormal =
        parts |> Seq.filter _.IsNormal |> Seq.exists (value >> containsABBA)

    let abbaInHyper =
        parts |> Seq.filter _.IsHypernet |> Seq.exists (value >> containsABBA)

    abbaInNormal && not abbaInHyper

System.IO.File.ReadAllLines "input/07.txt"
|> Seq.filter supportsTLS
|> Seq.length
|> printfn "Part 1: %i"
