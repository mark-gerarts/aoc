module AoC2015.Day10

let splitOnDifference xs =
    let rec go (xs: 'a list) (allGroups: ('a list) list) =
        match xs with
        | [] -> List.rev allGroups
        | x :: _ ->
            let group = List.takeWhile ((=) x) xs
            let tail = List.skipWhile ((=) x) xs

            go tail (group :: allGroups)

    go xs []

let step sequence =
    let groups = splitOnDifference sequence

    let lookAndSay group =
        let digit = List.head group
        let count = List.length group
        [ count; digit ]

    groups |> List.collect lookAndSay

let solve input n =
    Seq.init n (fun _ -> 0)
    |> Seq.fold (fun sequence _ -> step sequence) input
    |> Seq.length

let run filename =
    let input =
        filename
        |> System.IO.File.ReadLines
        |> Seq.head
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toList

    printfn "Part 1: %i" <| solve input 40
    printfn "Part 2: %i" <| solve input 50
