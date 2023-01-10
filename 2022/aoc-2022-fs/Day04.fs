module AoC2022.Day04

// Parse a range, e.g. "3-7"
let parseRange (range: string) =
    Set.ofList
    <| match range.Split '-' with
       | [| from; until |] -> [ int from .. int until ]
       | _ -> failwith "Invalid range"

let parseLine (line: string) = line.Split ',' |> Seq.map parseRange

let solve filterFn =
    System.IO.File.ReadAllLines("./input/04.txt")
    |> Seq.map parseLine
    |> Seq.filter (fun sets -> filterFn (Seq.head sets) (Seq.last sets))
    |> Seq.length

let run =
    let filterFnA s1 s2 =
        (Set.isSuperset s1 s2) || (Set.isSubset s1 s2)

    let filterFnB s1 s2 =
        Set.intersect s1 s2 |> Set.isEmpty |> not

    printfn "Part A: %i" <| solve filterFnA
    printfn "Part B: %i" <| solve filterFnB
