module AoC2016.Day01

type Action =
    | Left of int
    | Right of int

type Direction =
    | North
    | East
    | South
    | West

type Pos = int * int

type State = Pos list * Direction

let changeDirection currentDir action =
    let directions = [| North; East; South; West |]
    let currentIndex = Array.findIndex ((=) currentDir) directions

    let nextIndex =
        match action with
        | Left _ -> currentIndex - 1
        | Right _ -> currentIndex + 1

    directions[(nextIndex + 4) % 4]

let amount action =
    match action with
    | Left amount -> amount
    | Right amount -> amount

let moveOne ((x, y), dir) amount =
    match dir with
    | North -> (x, y + amount)
    | South -> (x, y - amount)
    | East -> (x + amount, y)
    | West -> (x - amount, y)

let rec moveN (pos, dir) amount =
    if amount = 0 then
        pos
    else
        pos :: [moveOne pos dir]

let applyAction (pos, dir) action =
    let newDir = changeDirection dir action
    let currentPos = List.head pos

    let newPos = move (pos, newDir) (amount action)
    (newPos, newDir)

let parse (input: string) =
    let parseAction (action: string) =
        let amount = action.Substring(1) |> int

        match action[0] with
        | 'L' -> Left amount
        | 'R' -> Right amount
        | c -> failwithf "Invalid dir %c" c

    input.Trim().Split(", ") |> Seq.map parseAction

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let run filename =
    let actions = filename |> System.IO.File.ReadAllText |> parse
    let path = actions |> Seq.scan applyAction ((0, 0), North) |> Seq.map fst

    path |> Seq.last |> distance (0, 0) |> printfn "Part 1: %i"

    path
    |> Seq.scan
        (fun (seen, _) p ->
            if Set.contains p seen then
                (seen, Some p)
            else
                (Set.add p seen, None))
        (Set.empty, None)
    |> Seq.toList
    |> printfn "%A"
(* |> Seq.pick snd
    |> distance (0, 0)
    |> printfn "Part 2: %A" *)
