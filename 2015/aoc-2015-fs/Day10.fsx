let step sequence =
    let rec go xs allGroups =
        match xs with
        | [] -> allGroups
        | x :: _ ->
            let count = List.takeWhile ((=) x) xs |> List.length
            let tail = List.skipWhile ((=) x) xs

            go tail (count :: x :: allGroups)

    go (List.rev sequence) []

let solve input n =
    { 1..n } |> Seq.fold (fun sequence _ -> step sequence) input |> Seq.length

let input =
    System.IO.File.ReadLines "input/10.txt"
    |> Seq.head
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.toList

printfn "Part 1: %i" <| solve input 40
printfn "Part 2: %i" <| solve input 50
