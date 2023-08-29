module AoC2015.Day05

open System.Text.RegularExpressions

let contains3vowels (input: string) =
    Regex.Replace(input, "[^aeoiu]", "") |> String.length |> ((<=) 3)

let containsDup (input: string) =
    input |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)

let notContainsForbidden (input: string) =
    let forbidden = [| "ab"; "cd"; "pq"; "xy" |]

    forbidden |> Array.exists input.Contains |> not

let containsTwoPairs (input: string) =
    let isOkPair (pair: string) =
        let firstOccurrence = input.IndexOf pair
        let lastOccurrence = input.LastIndexOf pair
        let diff = lastOccurrence - firstOccurrence

        diff <> 0 && diff > 1

    input |> Seq.windowed 2 |> Seq.map System.String |> Seq.exists isOkPair

let containsRepeatWithSpace (input: string) =
    let isRepeatWithSpace i =
        match i with
        | [| x; _; y |] when x = y -> true
        | _ -> false

    input |> Seq.windowed 3 |> Seq.exists isRepeatWithSpace

let isNice conditions input =
    conditions |> Array.forall (fun f -> f input)

let solve conditions file =
    file |> System.IO.File.ReadLines |> Seq.filter (isNice conditions) |> Seq.length

let run file =
    let conditionsPart1 = [| contains3vowels; containsDup; notContainsForbidden |]
    let conditionsPart2 = [| containsTwoPairs; containsRepeatWithSpace |]

    file |> solve conditionsPart1 |> printfn "Part 1: %i"
    file |> solve conditionsPart2 |> printfn "Part 2: %i"
