module AoC2022.Day17

open System

type Shape = list<bool>

type Direction =
    | Left
    | Right
    | Down

type Pos = int * int

type Grid = Set<Pos>

type MovementResult =
    | Blocked
    | Moved of Pos

let rec intListToBoolList intList =
    match intList with
    | [] -> []
    | x :: xs when x = 0 -> false :: intListToBoolList xs
    | _ :: xs -> true :: intListToBoolList xs

let line = [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1 ] |> intListToBoolList

let plus = [ 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 1; 0; 0; 1; 0; 0 ] |> intListToBoolList

let corner = [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 1; 1; 1; 0 ] |> intListToBoolList

let pipe = [ 1; 0; 0; 0; 1; 0; 0; 0; 1; 0; 0; 0; 1; 0; 0; 0 ] |> intListToBoolList

let block = [ 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0; 0 ] |> intListToBoolList

let rec overlaps shape1 shape2 =
    match shape1, shape2 with
    | [], [] -> false
    | _, []
    | [], _ -> failwith "Uneven lists provided"
    | x :: _, y :: _ when x && y -> true
    | _ :: xs, _ :: ys -> overlaps xs ys

let extractSlice grid (x, y) =
    // left->right, top->bottom extraction, with pos at bottom left.
    seq {
        for iy in { 0..3 } do
            let y' = y + 3 - iy

            for ix in { 0..3 } do
                let x' = x + ix

                if x' = 0 || x' = 8 then true // Left or right wall.
                elif y' = 0 then true // Floor.
                else Set.contains (x', y') grid
    }
    |> Seq.toList

let tryMove grid shape (x, y) dir =
    let newPos =
        match dir with
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)
        | Down -> (x, y - 1)

    let newSlice = extractSlice grid newPos

    if overlaps newSlice shape then Blocked else Moved newPos

let height grid =
    if Set.isEmpty grid then
        0
    else
        grid |> Set.map snd |> Set.maxElement

let dropPosition grid = (3, height grid + 4)

let parseInput =
    let parseChar c =
        match c with
        | '<' -> Left
        | '>' -> Right
        | _ -> failwith "Unexpected input character"

    System.IO.File.ReadAllText("./input/17.sample.txt").Trim()
    |> Seq.map parseChar
    |> Seq.toArray

let add grid shape (x, y) =
    let intToPos i = (x + i % 4, y + 3 - i / 4)

    Seq.zip { 0..15 } shape
    |> Seq.filter (fun (_, v) -> v)
    |> Seq.map (fst >> intToPos)
    |> Seq.fold (fun g p -> Set.add p g) grid

let printGrid grid shape pos =
    let g = add grid shape pos
    let width = 7
    let height = height g

    for i in { 0..height } do
        let y = height - i

        { 1..width }
        |> Seq.map (fun x -> if Set.contains (x, y) g then '#' else '.')
        |> String.Concat
        |> printfn "%i %s" y

    printfn ""

let partA =
    let winds = parseInput
    let shapes = [| line; plus; corner; pipe; block |]

    // Using lazy seqs was not performant.
    let incWindCounter wc =
        match wc + 1 with
        | x when x >= winds.Length -> 0
        | x -> x

    let incShapeCounter sc =
        match sc + 1 with
        | x when x >= shapes.Length -> 0
        | x -> x

    let rec solve grid shapesPlaced currentShape currentPos wc sc =
        if shapesPlaced >= 2022 then
            height grid
        else
            let newWc = incWindCounter wc
            let wind = winds[newWc]

            let newPos =
                match tryMove grid currentShape currentPos wind with
                | Blocked -> currentPos
                | Moved p -> p

            match tryMove grid currentShape newPos Down with
            | Moved p -> solve grid shapesPlaced currentShape p newWc sc
            | Blocked ->
                let newSc = incShapeCounter sc
                let newShape = shapes[newSc]
                let newGrid = add grid currentShape newPos

                solve newGrid (shapesPlaced + 1) newShape (dropPosition newGrid) newWc newSc

    let grid = Set.empty

    solve grid 0 shapes[0] (dropPosition grid) -1 0


let run = printfn "%A" partA
