let parseMove c =
    match c with
    | '^' -> 0, +1
    | 'v' -> 0, -1
    | '>' -> +1, 0
    | '<' -> -1, 0
    | _ -> failwithf "Invalid character %c" c

let applyInstructions instructions =
    let applyMove (seen, (x, y)) (dx, dy) =
        let newPosition = x + dx, y + dy
        Set.add newPosition seen, newPosition

    instructions |> Seq.fold applyMove (Set.singleton (0, 0), (0, 0)) |> fst

let input =
    System.IO.File.ReadAllText "input/03.txt" |> _.Trim() |> Seq.map parseMove

input |> applyInstructions |> Set.count |> printfn "Part 1: %i"

input
|> Seq.indexed
|> Seq.groupBy (fun (i, _) -> i % 2 = 0)
|> Seq.map (snd >> Seq.map snd >> applyInstructions)
|> Set.unionMany
|> Set.count
|> printfn "Part 2: %i"
