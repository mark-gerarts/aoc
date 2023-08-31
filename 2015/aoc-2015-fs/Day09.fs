module AoC2015.Day09

open FParsec

type InputLine = { src: string; dst: string; cost: int }

type DistanceMap = Map<(string * string), int>

let parseLine line =
    let pCity = regex "[A-Za-z]+"

    let pLine =
        pipe3 pCity (pstring " to " >>. pCity) (pstring " = " >>. pint32) (fun a b d -> { src = a; dst = b; cost = d })

    match run pLine line with
    | Success(distance, _, _) -> distance
    | _ -> failwithf "Could not parse '%s'" line

let addToDistanceMap distanceMap inputLine =
    distanceMap
    |> Map.add (inputLine.src, inputLine.dst) inputLine.cost
    |> Map.add (inputLine.dst, inputLine.src) inputLine.cost

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
            let distance = Map.find (a, b) distanceMap
            go (b :: tail) (total + distance)
        | _ -> total

    go cityList 0

let run filename =
    // Parse the input into a map, making it easy to query a distance when given
    // two cities.
    let distanceMap =
        filename
        |> System.IO.File.ReadLines
        |> Seq.map parseLine
        |> Seq.fold addToDistanceMap Map.empty

    // Get the list of unique cities.
    let cities = distanceMap |> Map.keys |> Seq.map fst |> Seq.distinct

    // Basically brute-force this by calculating the total distance of every
    // permutation of the city list.
    let distances =
        cities |> Seq.toList |> permute |> List.map (totalDistance distanceMap)

    printfn "Part 1: %i" (List.min distances)
    printfn "Part 2: %i" (List.max distances)
