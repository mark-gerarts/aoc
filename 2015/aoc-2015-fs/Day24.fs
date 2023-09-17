module AoC2015.Day24

let quantumEntanglement = List.reduce (fun a b -> a * b)

let rec passengerCompartments input desiredSum =
    let rec go maxGroupSize (group: uint64 list) xs =
        seq {
            match xs with
            | _ when List.length group > maxGroupSize -> ()
            | _ when List.sum group = desiredSum && List.length group = maxGroupSize -> yield group
            | [] -> ()
            | x :: xs ->
                yield! go maxGroupSize (x :: group) xs
                yield! go maxGroupSize group xs
        }

    let rec tryWithIncreasingSize size =
        let result = go size [] input

        if Seq.isEmpty result then
            tryWithIncreasingSize (size + 1)
        else
            result

    tryWithIncreasingSize 1

let solve filename numGroups =
    let input = filename |> System.IO.File.ReadAllLines |> Seq.map uint64 |> Seq.toList
    let desiredSum = List.sum input / (uint64 numGroups)

    passengerCompartments input desiredSum |> Seq.map quantumEntanglement |> Seq.min

let run filename =
    solve filename 3 |> printfn "Part 1: %i"
    solve filename 4 |> printfn "Part 2: %i"
