type Graph = Map<string, string list>

let parseLine (line: string) =
    match line.Split ": " with
    | [| node; outputNodes |] -> node, outputNodes.Split " " |> Array.toList
    | _ -> failwithf "Cannot parse line '%s'" line

// Straightforward DFS.
let part1 graph start destination =
    let rec go searchSpace pathCount =
        match searchSpace with
        | [] -> pathCount
        | (current, _) :: others when current = destination -> go others (pathCount + 1)
        | (current, seen) :: others ->
            match Map.tryFind current graph with
            | None -> go others pathCount
            | Some outNodes ->
                let expandedPaths =
                    outNodes
                    |> List.filter (fun node -> not <| Set.contains node seen)
                    |> List.map (fun node -> node, Set.add node seen)

                go (expandedPaths @ others) pathCount

    go [ (start, Set.singleton start) ] 0

let nodes graph =
    graph
    |> Map.toSeq
    |> Seq.collect (fun (node, outNodes) -> node :: outNodes)
    |> Seq.distinct

let updateAt f map key = Map.change key (Option.map f) map

let subgraph graph start =
    let rec go subGraph frontier =
        match frontier with
        | [] -> subGraph
        | node :: nodes ->
            let neighbours = Map.tryFind node graph |> Option.defaultValue []
            let newGraph = Map.add node neighbours subGraph
            go newGraph (nodes @ neighbours)

    go Map.empty [ start ]

let topologicalOrdening (graph: Graph) =
    let reverseGraph =
        Map.toSeq graph
        |> Seq.collect (fun (src, dstList) -> Seq.map (fun n -> n, src) dstList)
        |> Seq.groupBy fst
        |> Seq.map (fun (src, grouped) -> src, Seq.map snd grouped |> Seq.toList)
        |> Map.ofSeq

    let indegrees =
        nodes graph
        |> Seq.map (fun n -> n, Map.tryFind n reverseGraph |> Option.defaultValue [] |> List.length)
        |> Map.ofSeq

    let rec go indegrees ordening =
        let nextNode =
            indegrees
            |> Map.tryPick (fun node indegree -> if indegree = 0 then Some node else None)

        match nextNode with
        | None -> List.rev ordening
        | Some node ->
            let neighbours = Map.tryFind node graph |> Option.defaultValue []

            let newIndegrees =
                List.fold (updateAt (fun x -> x - 1)) (Map.remove node indegrees) neighbours

            let newOrdening = node :: ordening

            go newIndegrees newOrdening

    go indegrees []

let pathCount graph start dest =

    printfn "Starting path count..."

    let subGraph = subgraph graph start

    printfn "Got subgraph"

    let rec getWayCounts sortedNodes wayCounts =
        match sortedNodes with
        | [] -> wayCounts
        | node :: nodes ->
            let neighbours = Map.find node subGraph
            let wayCount = Map.tryFind node wayCounts |> Option.defaultValue 0
            let newWayCounts = List.fold (updateAt ((+) wayCount)) wayCounts neighbours

            getWayCounts nodes newWayCounts

    let initialCounts = nodes subGraph |> Seq.map (fun n -> n, 0) |> Map.ofSeq

    let pathCount =
        getWayCounts (topologicalOrdening subGraph) (Map.add start 1 initialCounts)
        |> Map.tryFind dest
        |> Option.defaultValue 0

    printfn "Pathcount %s->%s: %A" start dest pathCount

    pathCount



let graph =
    System.IO.File.ReadLines "input/11.txt" |> Seq.map parseLine |> Map.ofSeq

//pathCount graph "you" "out" |> printfn "Part 1: %i"

pathCount graph "svr" "fft" |> printfn "%A"

(*
async {
    let! svr2fft = async { return pathCount graph "svr" "fft" }
    and! svr2dac = async { return pathCount graph "svr" "dac" }
    and! fft2out = async { return pathCount graph "fft" "out" }
    and! dac2out = async { return pathCount graph "dac" "out" }
    and! dac2fft = async { return pathCount graph "dac" "fft" }
    and! fft2dac = async { return pathCount graph "fft" "dac" }

    let possiblePaths = svr2fft * fft2dac * dac2out + svr2dac * dac2fft * fft2out
    printfn "%A" possiblePaths
}
*)
