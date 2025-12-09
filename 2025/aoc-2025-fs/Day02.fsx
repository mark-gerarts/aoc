#r "nuget: FsUnit,7.1.1"
#nowarn "57" // For Array.Parallel being experimental right now.

open FsUnit

let inputIds =
    let parseRange (range: string) =
        match range.Split "-" with
        | [| lower; upper |] -> int64 lower, int64 upper
        | _ -> failwithf "Could not parse range %s" range

    let ranges =
        System.IO.File.ReadAllText "input/02.txt" |> _.Split(",") |> Seq.map parseRange

    seq {
        for lower, upper in ranges do
            yield! [ lower..upper ]
    }
    |> Seq.filter ((<) 10)
    |> Seq.toArray

let splits id =
    let id = string id

    seq {
        for numParts in { 2 .. id.Length } do
            Seq.splitInto numParts id |> Seq.distinct |> Seq.length |> (=) 1
    }

let isRepeatedTwice id = splits id |> Seq.head
let isRepeatedAtLeastTwice id = splits id |> Seq.exists ((=) true)

inputIds
|> Array.Parallel.filter isRepeatedTwice
|> Array.Parallel.sum
|> printfn "Part 1: %i"

inputIds
|> Array.Parallel.filter isRepeatedAtLeastTwice
|> Array.Parallel.sum
|> printfn "Part 2: %i"

isRepeatedTwice 12 |> should equal false
isRepeatedTwice 1212 |> should equal true
isRepeatedTwice 121212 |> should equal false
isRepeatedAtLeastTwice 121212 |> should equal true
isRepeatedAtLeastTwice 111 |> should equal true
isRepeatedAtLeastTwice 121 |> should equal false
