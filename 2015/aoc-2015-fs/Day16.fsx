open System.Text.RegularExpressions

let parseLine (line: string) =
    let attributeLine = Regex.Replace(line, "Sue (\d+):", "Sue: $1,")

    seq {
        for attribute in attributeLine.Split ", " do
            match attribute.Split ": " with
            | [| key; value |] -> key, int value
            | _ -> failwith "Invalid input line"
    }
    |> Map.ofSeq

let isCandidate operationMap scanResult aunt =
    let matches attribute value =
        match Map.tryFind attribute aunt, Map.find attribute operationMap with
        | Some auntValue, op -> op auntValue value
        | None, _ -> true

    Map.forall matches scanResult

let input = System.IO.File.ReadAllLines "input/16.txt" |> Seq.map parseLine

let scanResult =
    [ "children", 3
      "cats", 7
      "samoyeds", 2
      "pomeranians", 3
      "akitas", 0
      "vizslas", 0
      "goldfish", 5
      "trees", 3
      "cars", 2
      "perfumes", 1 ]
    |> Map.ofList

let operationMap = Map.map (fun _ _ -> (=)) scanResult

let operationMapPart2 =
    operationMap
    |> Map.add "cats" (>)
    |> Map.add "pomeranians" (<)
    |> Map.add "goldfish" (<)
    |> Map.add "trees" (>)

input
|> Seq.find (isCandidate operationMap scanResult)
|> Map.find "Sue"
|> printfn "Part 1: %A"

input
|> Seq.find (isCandidate operationMapPart2 scanResult)
|> Map.find "Sue"
|> printfn "Part 2: %A"
