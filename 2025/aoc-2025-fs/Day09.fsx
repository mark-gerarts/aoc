#nowarn "57" // For Array.Parallel being experimental right now.

type Orientation =
    | Colinear
    | Clockwise
    | CounterClockwise

let parseLine (line: string) =
    match line.Split "," with
    | [| x; y |] -> int64 x, int64 y
    | _ -> failwithf "Invalid line %s" line

let coords =
    System.IO.File.ReadLines "input/09.txt" |> Seq.map parseLine |> Seq.cache

let size ((x1, y1), (x2, y2)) =
    (abs (x2 - x1) + 1L) * (abs (y2 - y1) + 1L)

Seq.allPairs coords coords |> Seq.map size |> Seq.max |> printfn "Part 1: %i"

// https://www.geeksforgeeks.org/dsa/check-if-two-given-line-segments-intersect/
let intersects (p1, p2) (q1, q2) =
    let onSegment (x, y) ((x1, y1), (x2, y2)) =
        x <= max x1 x2 && x >= min x1 x2 && y <= max y1 y2 && y >= min y1 y2

    let orientation (x1, y1) (x2, y2) (x3, y3) =
        match (y2 - y1) * (x3 - x2) - (x2 - x1) * (y3 - y2) with
        | slope when slope = 0L -> Colinear
        | slope when slope < 0L -> CounterClockwise
        | _ -> Clockwise

    let o1 = orientation p1 p2 q1
    let o2 = orientation p1 p2 q2
    let o3 = orientation q1 q2 p1
    let o4 = orientation q1 q2 p2

    o1 <> o2 && o3 <> o4
    || o1 = Colinear && onSegment q1 (p1, p2)
    || o2 = Colinear && onSegment q2 (p1, p2)
    || o3 = Colinear && onSegment p1 (q1, q2)
    || o4 = Colinear && onSegment p2 (q1, q2)

let polygon =
    coords |> Seq.pairwise |> Seq.append [ Seq.last coords, Seq.head coords ]

let xs = coords |> Seq.map fst |> Set.ofSeq
let ys = coords |> Seq.map snd |> Set.ofSeq

let sidePoints ((x1, y1), (x2, y2)) =
    seq {
        // Yield other corners first as a heuristic
        yield x1, y2
        yield x2, y1

        for x in [ min x1 x2 .. max x1 x2 ] do
            if Set.contains x xs then
                yield x, y1
                yield x, y2

        for y in [ min y1 y2 .. max y1 y2 ] do
            if Set.contains y ys then
                yield x1, y
                yield x2, y
    }

let isInPolygon (p1, p2) =
    let pointInPolygon p =
        let origin = 0L, 0L

        let pointOnEdge = Seq.exists (intersects (p, p)) polygon

        let pointInside =
            Seq.filter (intersects (origin, p)) polygon
            |> Seq.length
            |> fun l -> l % 2 = 1

        pointOnEdge || pointInside

    //sidePoints (p1, p2) |> Seq.toArray |> Array.Parallel.forall pointInPolygon
    sidePoints (p1, p2) |> Seq.forall pointInPolygon

Seq.allPairs coords coords
|> Seq.map (fun square -> square, size square)
|> Seq.sortByDescending snd
|> Seq.toArray
|> Array.Parallel.tryFind (fst >> isInPolygon)
|> (Option.get >> snd)
|> printfn "Part 2: %i"


// NOT: 4652231070 (too high)
// NOT: 4472716059 (too high)
// NOT: 12128 (too low)
