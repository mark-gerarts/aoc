module AoC2023.Day01

type Direction =
    | Left
    | Right

let replaceDigitWords direction (line: string) =
    let tryReplace (line: string) =
        line
            .Replace("one", "1")
            .Replace("two", "2")
            .Replace("three", "3")
            .Replace("four", "4")
            .Replace("five", "5")
            .Replace("six", "6")
            .Replace("seven", "7")
            .Replace("eight", "8")
            .Replace("nine", "9")

    let charsToString cs = cs |> Array.ofList |> System.String

    // Take larger and larger prefixes to try and replace on.
    let rec goFromLeft prefix suffix =
        match suffix with
        | [] -> charsToString prefix
        | c :: cs ->
            let prefix = charsToString (prefix @ [ c ])
            goFromLeft (tryReplace prefix |> Seq.toList) cs

    // Similar, but taking larger and larger suffixes.
    let rec goFromRight prefix suffix =
        match prefix with
        | [] -> charsToString suffix
        | _ ->
            let last = List.last prefix
            let init = List.take (List.length prefix - 1) prefix
            let suffix = charsToString (last :: suffix)
            goFromRight init (tryReplace suffix |> Seq.toList)

    match direction with
    | Left -> goFromLeft [] (line |> Seq.toList)
    | Right -> goFromRight (line |> Seq.toList) []

let firstDigit line =
    line |> Seq.find System.Char.IsDigit |> (fun c -> int c - int '0')

let solve lefts rights =
    let leftSum = lefts |> Seq.sumBy firstDigit
    let rightSum = rights |> Seq.sumBy (Seq.rev >> firstDigit)

    leftSum * 10 + rightSum

let run filename =
    let lines = filename |> System.IO.File.ReadAllLines

    solve lines lines |> printfn "Part 1: %i"

    let lefts = lines |> Seq.map (replaceDigitWords Left)
    let rights = lines |> Seq.map (replaceDigitWords Right)

    solve lefts rights |> printfn "Part 2: %i"
