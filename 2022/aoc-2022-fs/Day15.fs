module AoC2022.Day15

open System.Text.RegularExpressions

type Pos = int * int

type SensorBeaconPair =
    { sensor: Pos
      beacon: Pos
      radius: int }

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parseLine line =
    let regex =
        Regex("Sensor at x=(?<x1>.+), y=(?<y1>.+): closest beacon is at x=(?<x2>.+), y=(?<y2>.+)")

    let result = regex.Matches(line)[0]
    let groups = result.Groups

    let ps = (int groups["x1"].Value, int groups["y1"].Value)
    let pb = (int groups["x2"].Value, int groups["y2"].Value)
    let r = distance ps pb

    { sensor = ps; beacon = pb; radius = r }

let parseInput =
    System.IO.File.ReadAllLines("./input/15.txt") |> Seq.map parseLine |> Seq.toList

let isInCoveredArea p sbPair =
    distance p sbPair.sensor <= sbPair.radius

let rec isCovered p allPairs =
    match allPairs with
    | [] -> false
    | x :: _ when isInCoveredArea p x -> true
    | _ :: xs -> isCovered p xs

let rec isEmptySquare p allPairs =
    match allPairs with
    | [] -> true
    | x :: _ when x.sensor = p || x.beacon = p -> false
    | _ :: xs -> isEmptySquare p xs

let dimensions allPairs =
    let largestRadius = allPairs |> Seq.map (fun p -> p.radius) |> Seq.max

    let xs =
        allPairs
        |> Seq.collect (fun sbPair -> [ sbPair.beacon; sbPair.sensor ])
        |> Seq.map fst

    let minX = Seq.min xs
    let maxX = Seq.max xs

    (minX - largestRadius, maxX + largestRadius)

let partA allPairs =
    let (minX, maxX) = dimensions allPairs
    let targetY = 2000000

    { minX..maxX }
    |> Seq.map (fun x -> x, targetY)
    |> Seq.filter (fun p -> isEmptySquare p allPairs)
    |> Seq.filter (fun p -> isCovered p allPairs)
    |> Seq.length

let partB allPairs =
    let min = 0
    let max = 4000000

    // Naive search with (x,y) ranging from min to max takes way too long, so we
    // limit our search space to coordinates right next to the edge of the area
    // of a sensor. This works because we know the solution we're looking for is
    // a unique point.
    let getExclusiveEdge sbPair =
        let (sx, sy) = sbPair.sensor
        let radius = sbPair.radius

        seq {
            for i in { -radius .. radius } do
                let y = sy + i
                let leftX = sx - (radius - abs i)
                let rightX = sx + (radius - abs i)

                yield (leftX - 1, y)
                yield (rightX + 1, y)

                if i = -radius then
                    yield (leftX, y - 1)

                if i = radius then
                    yield (leftX, y + 1)
        }

    Seq.collect getExclusiveEdge allPairs
    |> Seq.filter (fun (x, y) -> x >= min && x <= max && y >= min && y <= max)
    |> Seq.find (fun p -> isEmptySquare p allPairs && not (isCovered p allPairs))
    |> (fun (x, y) -> (bigint x) * (bigint max) + (bigint y))

let run =
    let allPairs = parseInput

    printfn "Part A: %i" <| partA allPairs
    printfn "Part B: %A" <| partB allPairs
