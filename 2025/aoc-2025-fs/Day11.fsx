let parseLine (line: string) =
    match line.Split ": " with
    | [| node; outputNodes |] -> node, outputNodes.Split " "
    | _ -> failwithf "Cannot parse line '%s'" line

let graph =
    System.IO.File.ReadLines "input/11.txt" |> Seq.map parseLine |> Map.ofSeq

let allPaths graph start destination =
    let rec go searchSpace pathCount =
        match searchSpace with
        | [] -> pathCount
        | (current, seen) :: others when current = destination -> go others (pathCount + 1)
        | (current, seen) :: others ->
            let expandedPaths =
                Map.find current graph
                |> List.ofArray
                |> List.filter (fun node -> not <| Set.contains node seen)
                |> List.map (fun node -> node, Set.add node seen)

            go (expandedPaths @ others) pathCount

    go [ (start, Set.singleton start) ] 0

allPaths graph "you" "out" |> printfn "%A"
