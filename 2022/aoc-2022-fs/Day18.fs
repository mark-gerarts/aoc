module AoC2022.Day18

type Coord = { X: int; Y: int; Z: int }

type Face =
    | Left
    | Right
    | Top
    | Bottom
    | Front
    | Back

type Side = Coord * Face

let parseLine (line: string) =
    let parts = line.Split(',') |> Array.map int

    { X = parts[0]
      Y = parts[1]
      Z = parts[2] }

let parseInput = System.IO.File.ReadLines "./input/18.txt" |> Seq.map parseLine

// When given a side (= coord + face), normalizes to the combination whose
// coordinates are closest to 0. E.g. (2,1,1 Left) becomes (1,1,1 Right)
let normalize (coord, face) =
    match face with
    | Left when coord.X > 0 -> { coord with X = coord.X - 1 }, Right
    | Right when coord.X < 0 -> { coord with X = coord.X + 1 }, Left
    | Top when coord.Y > 0 -> { coord with Y = coord.Y - 1 }, Bottom
    | Bottom when coord.Y < 0 -> { coord with Y = coord.Y + 1 }, Top
    | Front when coord.Z > 0 -> { coord with Z = coord.Z - 1 }, Back
    | Back when coord.Z < 0 -> { coord with Z = coord.Z + 1 }, Front
    | _ -> coord, face

let withAllSides coord =
    [ Left; Right; Top; Bottom; Front; Back ] |> List.map (fun side -> coord, side)

let countAllVisibleSides coords =
    coords
    |> Seq.collect withAllSides
    |> Seq.map normalize
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count = 1)
    |> Seq.length

let partA = parseInput |> countAllVisibleSides

let extrema (coords: Coord seq) =
    let xs = coords |> Seq.map (fun c -> c.X)
    let ys = coords |> Seq.map (fun c -> c.Y)
    let zs = coords |> Seq.map (fun c -> c.Z)

    {| minX = xs |> Seq.min
       maxX = xs |> Seq.max
       minY = ys |> Seq.min
       maxY = ys |> Seq.max
       minZ = zs |> Seq.min
       maxZ = zs |> Seq.max |}

// Neighbours, without diagonals.
let neighbors coord =
    [ { coord with X = coord.X + 1 }
      { coord with X = coord.X - 1 }
      { coord with Y = coord.Y + 1 }
      { coord with Y = coord.Y - 1 }
      { coord with Z = coord.Z + 1 }
      { coord with Z = coord.Z - 1 } ]

let constructHull coords =
    let extrema = extrema coords
    let coordsSet = Set.ofSeq coords

    let start =
        { X = extrema.minX - 1
          Y = extrema.minY - 1
          Z = extrema.minZ - 1 }

    let isInBounds { X = x; Y = y; Z = z } =
        x >= extrema.minX - 1
        && x <= extrema.maxX + 1
        && y >= extrema.minY - 1
        && y <= extrema.maxX + 1
        && z >= extrema.minZ - 1
        && z <= extrema.maxZ + 1

    // Start at an exterior point and start BFSing until we've seen every
    // reachable point.
    let rec bfs queue seen =
        match queue with
        | [] -> seen
        | c :: cs ->
            let unseenNeighbours =
                neighbors c
                |> Seq.filter isInBounds
                |> Seq.filter (fun c -> not <| Set.contains c seen)
                |> Seq.filter (fun c -> not <| Set.contains c coordsSet)
                |> Seq.toList

            let newSeen = Set.union seen (Set.ofList unseenNeighbours)
            let newQueue = List.append cs unseenNeighbours

            bfs newQueue newSeen

    bfs [ start ] Set.empty

// The idea is to create a hull beam around the scene and then count the visible
// sides as in part A, but subtract the exterior sides.
let partB =
    let coords = parseInput
    let hull = constructHull coords

    let allVisibleSides = countAllVisibleSides hull

    let exteriorSides =
        let extrema = extrema hull

        let lx = 1 + extrema.maxX - extrema.minX |> abs
        let ly = 1 + extrema.maxY - extrema.minY |> abs
        let lz = 1 + extrema.maxZ - extrema.minZ |> abs

        (2 * (lx * ly)) + (2 * (lx * lz)) + (2 * (lz * ly))

    allVisibleSides - exteriorSides

let run =
    printfn "Part A: %i" partA
    printfn "Part B: %i" partB
