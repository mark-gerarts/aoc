open System.Text.RegularExpressions

let parseLine (line: string) =
    let parseDigits x =
        Regex.Matches(x, "\d+") |> Seq.map (_.Value >> int)

    match line.Split('|') |> Array.map parseDigits with
    | [| winning; actual |] -> Seq.tail winning, actual
    | _ -> failwithf "Failed to parse line %s" line

let getNumberOfMatches (winning, actual) =
    Set.intersect (Set.ofSeq winning) (Set.ofSeq actual)
    |> Set.count

let part1 scores =
    List.sumBy (fun x -> pown 2 (x - 1)) scores

let part2 scores =
    let rec go scores bonusCards totalCards =
        match scores, bonusCards with
        | [], _ -> totalCards
        | _, [] -> failwith "Out of bonuses"
        | score :: rest, bonus :: bonusCards ->
            let newHead = bonusCards |> List.take score |> List.map ((+) bonus)
            let newTail = bonusCards |> List.skip score

            go rest (newHead @ newTail) (totalCards + bonus)

    let bonusCards = List.replicate (List.length scores) 1
    go scores bonusCards 0

let scores =
    System.IO.File.ReadLines "input/04.txt"
    |> Seq.map (parseLine >> getNumberOfMatches)
    |> Seq.toList

scores |> part1 |> printfn "Part 1: %i"
scores |> part2 |> printfn "Part 2: %i"
