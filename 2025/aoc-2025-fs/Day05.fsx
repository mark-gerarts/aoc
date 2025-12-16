let parseRange (range: string) =
    match range.Split "-" |> Array.map int64 with
    | [| l; r |] -> l, r
    | _ -> failwithf "Invalid input range %s" range

let ranges, ids =
    let inputText = System.IO.File.ReadAllText "input/05.txt" |> _.Trim()

    match inputText |> _.Split("\n\n") with
    | [| rangesInput; idsInput |] ->
        let ranges = rangesInput.Split "\n" |> Seq.map parseRange |> Seq.sort
        let ids = idsInput.Split "\n" |> Seq.map int64
        ranges, ids
    | _ -> failwith "Could not parse input"

let contains id (lower, upper) = lower <= id && id <= upper

let isFresh id = Seq.exists (contains id) ranges

ids |> Seq.filter isFresh |> Seq.length |> printfn "Part 1: %i"

// We sort all ranges. Then this function does the heavy lifting by deciding if
// the ranges overlap, merging them together, or if they are two proper separate
// ranges.
let mergeConsecutiveRanges (l1, u1 as r1) (l2, u2 as r2) =
    if r1 = r2 then [ r1 ]
    elif l2 > u1 then [ r1; r2 ]
    else [ l1, max u1 u2 ]

ranges
|> Seq.fold
    (fun xs r ->
        match xs with
        | [] -> [ r ]
        | x :: xs -> List.rev (mergeConsecutiveRanges x r) @ xs)
    []
|> Seq.sumBy (fun (l, u) -> u - l + 1L)
|> printfn "Part 2: %i"
