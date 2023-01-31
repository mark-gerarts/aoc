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
    let mutable rooms = Map.empty

    for line in System.IO.File.ReadAllLines("./input/16.sample.txt") do
        let matches = Regex("[A-Z]{2}").Matches line

        let name = matches[0].Value

        let adjacentRooms =
            { 1 .. matches.Count - 1 } |> Seq.map (fun i -> matches[i].Value) |> Seq.toList

        let flowRateMatches = Regex("\d+").Matches line
        let flowRate = int flowRateMatches[0].Value

        let room =
            { name = name
              flowRate = flowRate
              adjacentRooms = adjacentRooms }

        rooms <- Map.add name room rooms

    rooms

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

// Reduce the search space to only rooms with a flow rate.
let pruneSearchSpace rooms =
    let roomsWithFlowRate =
        rooms
        |> Map.values
        |> Seq.filter (fun r -> r.flowRate > 0)
        |> Seq.map (fun r -> r.name)
        |> Seq.toList

    let interestingRooms = "AA" :: roomsWithFlowRate
    let mutable prunedSearchSpace = Map.empty

    for roomName in interestingRooms do
        let connections =
            interestingRooms
            |> List.filter (fun otherName -> otherName <> roomName && otherName <> "AA")
            |> List.map (fun otherName -> otherName, bfs rooms roomName otherName |> List.length)

        prunedSearchSpace <- Map.add roomName connections prunedSearchSpace

    prunedSearchSpace

let partA =
    let roomMap = parseInput
    let prunedMap = pruneSearchSpace roomMap
    let start = Map.find "AA" roomMap

    let mutable cache = Map.empty

    let rec search room openRooms minutesRemaining =
        let memoizeSearch room openRooms minutesRemaining =
            let stateAsKey = (room, openRooms, minutesRemaining)

            match Map.tryFind stateAsKey cache with
            | Some result -> result
            | None ->
                let result = search room openRooms minutesRemaining
                cache <- Map.add stateAsKey result cache
                result

        let handleOption option =
            match option with
            | OpenValve ->
                let newMinutesRemaining = minutesRemaining - 1
                let addedPressure = newMinutesRemaining * room.flowRate
                let newOpenRooms = Set.add room.name openRooms

                addedPressure + memoizeSearch room newOpenRooms newMinutesRemaining
            | MoveTo(name, time) ->
                let newRoom = Map.find name roomMap
                memoizeSearch newRoom openRooms (minutesRemaining - time)

        if minutesRemaining <= 0 then
            0
        else
            let results =
                possibleSteps room openRooms prunedMap minutesRemaining |> List.map handleOption

            match results with
            | [] -> 0
            | xs -> List.max xs

    search start Set.empty 30

let run = printfn "%A" <| partA
