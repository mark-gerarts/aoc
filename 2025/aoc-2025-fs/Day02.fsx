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
    |> Seq.toArray

let isRepeatedTwice id =
    let id = string id
    let left = id[0 .. id.Length / 2 - 1]

    left + left = id

let isRepeatedAtLeastTwice id =
    let id = string id
    let strlen = id.Length

    let repeats =
        seq {
            for i in [ strlen / 2 .. -1 .. 1 ] do
                if id = String.replicate (strlen / i) id[0 .. i - 1] then
                    yield true
        }

    Seq.contains true repeats

// The solution is okayish speed-wise, but since I don't have much F# async
// experience, let's make it multithreaded.
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
