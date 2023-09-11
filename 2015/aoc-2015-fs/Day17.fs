module AoC2015.Day17

type Container = { id: int; volume: int }

let rec fill target containers filled =
    seq {
        match containers with
        | x :: xs ->
            let filledVolume = filled |> List.sumBy (fun c -> c.volume)
            let newVolume = x.volume + filledVolume

            if newVolume = target then
                yield x :: filled

            if newVolume < target then
                yield! fill target xs (x :: filled)

            yield! fill target xs filled
        | _ -> ()
    }

let run filename =
    let containers =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map int
        |> Seq.mapi (fun i v -> { id = i; volume = v })
        |> Seq.toList

    let result = fill 150 containers []
    Seq.length result |> printfn "Part 1: %i"

    let minimumLength = result |> Seq.map Seq.length |> Seq.min

    result
    |> Seq.filter (fun xs -> Seq.length xs = minimumLength)
    |> Seq.length
    |> printfn "Part 2: %i"
