type Graph = Map<string, string list>

let parseLine (line: string) =
    match line.Split ": " with
    | [| node; outputNodes |] -> node, outputNodes.Split " " |> Array.toList
    | _ -> failwithf "Cannot parse line '%s'" line

let nodes graph =
    graph
    |> Map.toSeq
    |> Seq.collect (fun (node, outNodes) -> node :: outNodes)
    |> Seq.distinct

let updateAt f map key = Map.change key (Option.map f) map

// Given a graph and a start node, returns the subgraph containing nodes
// reachable from the start node.
let subgraph graph start =
    let rec go subGraph frontier =
        match frontier with
        | [] -> subGraph
        | node :: nodes when Map.containsKey node subGraph -> go subGraph nodes
        | node :: nodes ->
            let neighbours = Map.tryFind node graph |> Option.defaultValue []
            let newGraph = Map.add node neighbours subGraph
            go newGraph (nodes @ neighbours)

    go Map.empty [ start ]

let topologicalOrdening (graph: Graph) =
    // (node, outNodes) -> (node, inNodes)
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
        match indegrees |> Map.tryFindKey (fun _ indegree -> indegree = 0) with
        | None -> List.rev ordening
        | Some node ->
            let neighbours = Map.tryFind node graph |> Option.defaultValue []

            let newIndegrees =
                List.fold (updateAt (fun x -> x - 1)) (Map.remove node indegrees) neighbours

            go newIndegrees (node :: ordening)

    go indegrees []

let pathCount graph start dest =
    let subGraph = subgraph graph start

    let rec getWayCounts sortedNodes wayCounts =
        match sortedNodes with
        | [] -> wayCounts
        | node :: nodes ->
            let neighbours = Map.find node subGraph
            let wayCount = Map.tryFind node wayCounts |> Option.defaultValue 0L
            let newWayCounts = List.fold (updateAt ((+) wayCount)) wayCounts neighbours

            getWayCounts nodes newWayCounts

    let initialCounts = nodes subGraph |> Seq.map (fun n -> n, 0L) |> Map.ofSeq

    getWayCounts (topologicalOrdening subGraph) (Map.add start 1L initialCounts)
    |> Map.tryFind dest
    |> Option.defaultValue 0L

let graph =
    System.IO.File.ReadLines "input/11.txt" |> Seq.map parseLine |> Map.ofSeq

pathCount graph "you" "out" |> printfn "Part 1: %i"

let svr2fft = pathCount graph "svr" "fft"
let svr2dac = pathCount graph "svr" "dac"
let fft2out = pathCount graph "fft" "out"
let dac2out = pathCount graph "dac" "out"
let dac2fft = pathCount graph "dac" "fft"
let fft2dac = pathCount graph "fft" "dac"

let possiblePaths = svr2fft * fft2dac * dac2out + svr2dac * dac2fft * fft2out
printfn "Part 2: %i" possiblePaths
