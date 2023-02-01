module AoC2022.Day16

open System.Text.RegularExpressions

type Room =
    { name: string
      flowRate: int
      adjacentRooms: list<string> }

type Rooms = Map<string, Room>

type Action =
    | OpenValve
    | MoveTo of string * int

let parseInput =
    let parseLine line =
        let roomNames =
            Regex("[A-Z]{2}").Matches line |> Seq.map (fun m -> m.Value) |> Seq.toList

        let name = List.head roomNames
        let adjacentRooms = List.tail roomNames

        let flowRateMatches = Regex("\d+").Matches line
        let flowRate = int flowRateMatches[0].Value

        { name = name
          flowRate = flowRate
          adjacentRooms = adjacentRooms }

    System.IO.File.ReadAllLines("./input/16.sample.txt")
    |> Seq.map parseLine
    |> Seq.fold (fun rooms room -> Map.add room.name room rooms) Map.empty

let bfs rooms start target =
    let mutable parents = Map.empty

    let rec reconstruct c =
        match Map.tryFind c parents with
        | Some p -> c :: reconstruct p
        | None -> []

    let rec go queue seen =
        match queue with
        | [] -> failwith "This can't happen"
        | x :: _ when x = target -> reconstruct x
        | x :: xs ->
            let candidates =
                Map.find x rooms
                |> (fun r -> r.adjacentRooms)
                |> List.filter (fun name -> not <| Set.contains name seen)

            for candidate in candidates do
                parents <- Map.add candidate x parents

            let newQueue = List.append xs candidates
            let newSeen = Set.union seen (Set.ofList candidates)

            go newQueue newSeen

    go (List.singleton start) (Set.singleton start)

// Reduce the search space to only rooms with a flow rate, and construct a map
// with shortest distances to other rooms.
let pruneSearchSpace rooms =
    let roomsWithFlowRate =
        rooms
        |> Map.values
        |> Seq.filter (fun r -> r.flowRate > 0)
        |> Seq.map (fun r -> r.name)
        |> Seq.toList

    let interestingRooms = "AA" :: roomsWithFlowRate

    let getConnectionsForRoom roomName =
        interestingRooms
        |> List.filter (fun otherName -> otherName <> roomName && otherName <> "AA")
        |> List.map (fun otherName -> otherName, bfs rooms roomName otherName |> List.length)

    interestingRooms
    |> List.map (fun name -> (name, getConnectionsForRoom name))
    |> List.fold (fun m (name, cxs) -> Map.add name cxs m) Map.empty

let possibleSteps room openRooms prunedSearchSpace minutesRemaining =
    let moves =
        Map.find room.name prunedSearchSpace
        |> List.filter (fun (name, _) -> not <| Set.contains name openRooms)
        |> List.filter (fun (_, time) -> time < minutesRemaining)
        |> List.map (fun (name, time) -> MoveTo(name, time))

    if Set.contains room.name openRooms || room.name = "AA" then
        moves
    else
        OpenValve :: moves

let solve =
    let roomMap = parseInput
    let prunedMap = pruneSearchSpace roomMap
    let start = Map.find "AA" roomMap

    let mutable cache = Map.empty

    let rec search room openRooms minutesRemaining ele =
        let memoizeSearch room openRooms minutesRemaining ele =
            let stateAsKey = (room, openRooms, minutesRemaining, ele)

            match Map.tryFind stateAsKey cache with
            | Some result -> result
            | None ->
                let result = search room openRooms minutesRemaining ele
                cache <- Map.add stateAsKey result cache
                result

        let handleOption option =
            match option with
            | OpenValve ->
                let newMinutesRemaining = minutesRemaining - 1
                let addedPressure = newMinutesRemaining * room.flowRate
                let newOpenRooms = Set.add room.name openRooms

                addedPressure + memoizeSearch room newOpenRooms newMinutesRemaining ele
            | MoveTo(name, time) ->
                let newRoom = Map.find name roomMap
                memoizeSearch newRoom openRooms (minutesRemaining - time) ele

        if minutesRemaining <= 0 then
            0
        else
            let results =
                possibleSteps room openRooms prunedMap minutesRemaining |> List.map handleOption

            let eleMax = if ele then memoizeSearch start openRooms 26 false else 0

            match results with
            | [] -> max 0 eleMax
            | xs -> max (List.max xs) eleMax

    let partA = search start Set.empty 30 false
    let partB = search start Set.empty 26 true

    (partA, partB)

// Beware, part B runs prettyyy slow on the real input.
let run =
    let (partA, partB) = solve
    printfn "Part A: %i" partA
    printfn "Part B: %i" partB
