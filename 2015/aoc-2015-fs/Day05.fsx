open System.Text.RegularExpressions

let contains3vowels (input: string) =
    Regex.Replace(input, "[^aeoiu]", "") |> String.length |> (<=) 3

let containsDup (input: string) =
    input |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)

let notContainsForbidden (input: string) =
    let forbidden = [| "ab"; "cd"; "pq"; "xy" |]

    forbidden |> Array.forall (not << input.Contains)

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

let satisfies conditions input =
    List.forall (fun f -> f input) conditions

let solve conditions =
    System.IO.File.ReadLines "input/05.txt"
    |> Seq.filter (satisfies conditions)
    |> Seq.length

let conditionsPart1 = [ contains3vowels; containsDup; notContainsForbidden ]
let conditionsPart2 = [ containsTwoPairs; containsRepeatWithSpace ]

solve conditionsPart1 |> printfn "Part 1: %i"
solve conditionsPart2 |> printfn "Part 2: %i"
