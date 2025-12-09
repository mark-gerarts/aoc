let coords =
    let parseLine (line: string) =
        match line.Split "," |> Array.map int64 with
        | [| x; y; z |] -> x, y, z
        | _ -> failwithf "Invalid input line %s" line

    System.IO.File.ReadAllLines "input/08.txt" |> Seq.map parseLine

// No need for sqrt since we only care about relative distances.
let distance (x1, y1, z1) (x2, y2, z2) =
    pown (x2 - x1) 2 + pown (y2 - y1) 2 + pown (z2 - z1) 2

let sortedDistances =
    seq {
        for coord in coords do
            for otherCoord in coords do
                if otherCoord <> coord then
                    (coord, otherCoord), distance coord otherCoord
    }
    |> Seq.sortBy snd
    |> Seq.distinctBy snd
    |> Seq.cache

let connect circuits (box1, box2) =
    let popCircuitContaingElement circuits coord =
        match List.tryFindIndex (Set.contains coord) circuits with
        | Some index -> circuits[index], List.take index circuits @ List.skip (index + 1) circuits
        | None -> Set.singleton coord, circuits

    let c1, circuits = popCircuitContaingElement circuits box1
    let c2, circuits = popCircuitContaingElement circuits box2

    Set.union c1 c2 :: circuits

let initialCircuits = coords |> Seq.map Set.singleton |> Seq.toList

let steps = sortedDistances |> Seq.map fst |> Seq.scan connect initialCircuits

// 40 steps -> 23s
steps
|> Seq.item 1000
|> Seq.map Set.count
|> Seq.sortDescending
|> Seq.take 3
|> Seq.fold (*) 1
|> printfn "Part 1: %i"

let stepsUntilFullCircuit = steps |> Seq.findIndex (List.length >> (=) 1)

sortedDistances
|> Seq.map fst
|> Seq.item (stepsUntilFullCircuit - 1)
|> (fun ((x1, _, _), (x2, _, _)) -> x1 * x2)
|> printfn "Part 2: %i"
