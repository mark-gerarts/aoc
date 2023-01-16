module AoC2022.Day09

type Pos = int * int

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let updateSegment segment previousSegment =
    let update =
        match diff previousSegment segment with
        // 2 steps up, down, left, or right -> move 1 step in that direction.
        | (0, 2) -> (0, 1)
        | (0, -2) -> (0, -1)
        | (2, 0) -> (1, 0)
        | (-2, 0) -> (-1, 0)
        // Horse chess diagonal -> move 1 diagonal.
        | (1, 2) -> (1, 1)
        | (1, -2) -> (1, -1)
        | (-1, 2) -> (-1, 1)
        | (-1, -2) -> (-1, -1)
        | (2, 1) -> (1, 1)
        | (2, -1) -> (1, -1)
        | (-2, 1) -> (-1, 1)
        | (-2, -1) -> (-1, -1)
        // Regular diagonal -> move 1 diagonal.
        | (2, 2) -> (1, 1)
        | (2, -2) -> (1, -1)
        | (-2, 2) -> (-1, 1)
        | (-2, -2) -> (-1, -1)
        // Otherwise do nothing.
        | _ -> (0, 0)

    add segment update

let moveRope rope dir =
    let newRope =
        match rope with
        | [] -> failwith "Empty rope"
        | x :: xs -> add dir x :: xs

    let newTail =
        Seq.zip (List.tail newRope) newRope
        |> Seq.map (fun (segment, previousSegment) -> updateSegment segment previousSegment)
        |> Seq.toList

    (List.head newRope) :: newTail

let parseLine (line: string) =
    match line.Split ' ' with
    | [| "U"; amount |] -> (0, 1), int amount
    | [| "D"; amount |] -> (0, -1), int amount
    | [| "L"; amount |] -> (-1, 0), int amount
    | [| "R"; amount |] -> (1, 0), int amount
    | _ -> failwith $"Unable to parse line: {line}"

let solveWithLength length =
    let mutable rope = List.init length (fun _ -> (0, 0))
    let mutable seen = Set.empty

    let instructions = System.IO.File.ReadLines("./input/09.txt") |> Seq.map parseLine

    for (dir, amount) in instructions do
        for _ in { 0 .. amount - 1 } do
            rope <- moveRope rope dir
            seen <- Set.add (List.last rope) seen

    Set.count seen

let run =
    printfn "Part A: %i" <| solveWithLength 2
    printfn "Part B: %i" <| solveWithLength 10
