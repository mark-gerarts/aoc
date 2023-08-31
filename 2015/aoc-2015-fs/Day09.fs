module AoC2015.Day09

open FParsec

// For parsing
type InputLine = { src: string; dst: string; cost: int }

// Graph stuff
type Edge = { cost: int; dst: string }

and Node = { name: string; edges: Edge array }

type Graph = Map<string, Node>

let parseLine line =
    let pCity = regex "[A-Za-z]+"

    let pLine =
        pipe3 pCity (pstring " to " >>. pCity) (pstring " = " >>. pint32) (fun a b d -> { src = a; dst = b; cost = d })

    match run pLine line with
    | Success(distance, _, _) -> distance
    | _ -> failwithf "Could not parse '%s'" line

let createGraph graph inputLine =
    let insert src dst cost graph =
        let edge = { cost = cost; dst = dst }

        match Map.tryFind src graph with
        | Some node ->
            Map.add
                src
                { node with
                    edges = Array.append node.edges [| edge |] }
                graph
        | None -> Map.add src { name = src; edges = [| edge |] } graph

    graph
    |> insert inputLine.src inputLine.dst inputLine.cost
    |> insert inputLine.dst inputLine.src inputLine.cost

// Int32.max doesn't work, because it doesn't respect Infinity + x
let infinity = 99999

// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
let calculateDistances (startNode: string) (graph: Graph) =
    let rec go currentNodeLabel visited distances =
        let currentNode = Map.find currentNodeLabel graph
        let currentDistance = Map.find currentNodeLabel distances

        let neighbours =
            currentNode.edges |> Array.filter (fun e -> Set.contains e.dst visited |> not)

        let updateIfLower distances edge =
            let oldDistance = Map.find edge.dst distances
            let newDistance = edge.cost + currentDistance

            if newDistance < oldDistance then
                Map.add edge.dst newDistance distances
            else
                distances

        let newDistances = neighbours |> Array.fold updateIfLower distances
        let newVisited = Set.add currentNodeLabel visited

        if Set.count visited = Map.count graph then
            newDistances
        else
            let nextNode =
                graph
                |> Map.keys
                |> Seq.filter (fun label -> Set.contains label visited |> not)
                |> Seq.minBy (fun label -> Map.find label newDistances)

            go nextNode newVisited newDistances

    let initialVisited = Set.empty

    let initialDistances =
        graph
        |> Map.keys
        |> Seq.map (fun label -> (label, infinity))
        |> Map.ofSeq
        |> Map.add startNode 0

    go startNode initialVisited initialDistances

// https://stackoverflow.com/a/3129136
let rec distribute e =
    function
    | [] -> [ [ e ] ]
    | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

let rec permute =
    function
    | [] -> [ [] ]
    | e :: xs -> List.collect (distribute e) (permute xs)

let totalDistance distanceMap cityList =
    let rec go cityList total =
        match cityList with
        | a :: b :: tail ->
            let distance = Map.find a distanceMap |> Map.find b
            go (b :: tail) (total + distance)
        | _ -> total

    go cityList 0

// TSP, yay.
let run filename =
    // Parse input to a graph
    let graph =
        filename
        |> System.IO.File.ReadLines
        |> Seq.map parseLine
        |> Seq.fold createGraph Map.empty

    let cities = Map.keys graph |> Seq.toList

    // For every city, calculate the distance to each other city.
    let distanceMaps =
        cities
        |> List.map (fun city -> (city, calculateDistances city graph))
        |> Map.ofList

    // Now throw efficiency overboard and hope the puzzle input is small enough:
    // list all possible orderings of cities, calculate the total distance, and
    // return the smallest.
    let distances = cities |> permute |> List.map (totalDistance distanceMaps)

    // TODO: part 2
    distances |> List.min |> printfn "Part 1: %i"
