module AoC2023.Day04

open System.Text.RegularExpressions

let parseLine (line: string) =
    let extractDigits line =
        [ for m in Regex.Matches(line, "\d+") -> int m.Value ]

    match (line.Split(':')[1]).Split('|') with
    | [| left; right |] -> extractDigits left, extractDigits right
    | _ -> failwithf "Incorrect input line %s" line

let matchingNumbers (winningNumbers, actualNumbers) =
    Set.intersect (Set.ofList winningNumbers) (Set.ofList actualNumbers)
    |> Set.count

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

let run filename =
    let scores =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map (parseLine >> matchingNumbers)

    scores |> Seq.sumBy (fun s -> pown 2 (s - 1)) |> printfn "Part 1: %i"
    scores |> Seq.toList |> part2 |> printfn "Part 2: %i"
