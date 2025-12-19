let rec fill target containers filled =
    seq {
        match containers with
        | volume :: containers ->
            let newVolume = volume + List.sum filled

            if newVolume = target then
                yield volume :: filled

            if newVolume < target then
                yield! fill target containers (volume :: filled)

            yield! fill target containers filled
        | _ -> ()
    }

let containers =
    System.IO.File.ReadAllLines "input/17.txt" |> Seq.map int |> Seq.toList

let result = fill 150 containers []
Seq.length result |> printfn "Part 1: %i"

let minimumLength = result |> Seq.map Seq.length |> Seq.min

result
|> Seq.filter (fun xs -> Seq.length xs = minimumLength)
|> Seq.length
|> printfn "Part 2: %i"
