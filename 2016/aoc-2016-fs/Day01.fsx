type Direction =
    | Left
    | Right
    | Same

type Orientation =
    | North
    | East
    | South
    | West

type State =
    { position: int * int
      orientation: Orientation
      seen: Set<int * int> }

let turn orientation direction =
    match orientation, direction with
    | North, Left -> West
    | North, Right -> East
    | East, Left -> North
    | East, Right -> South
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North
    | _, Same -> orientation

let step state direction =
    let newOrientation = turn state.orientation direction

    let x, y = state.position

    let newPos =
        match newOrientation with
        | North -> x, y + 1
        | East -> x + 1, y
        | South -> x, y - 1
        | West -> x - 1, y

    { position = newPos
      orientation = newOrientation
      seen = Set.add newPos state.seen }

let distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let parseLine (line: string) =
    let parseInstruction (instruction: string) =
        let direction = if instruction[0] = 'L' then Left else Right
        direction, int (instruction[1..])

    line.Split ", " |> Seq.map parseInstruction |> Seq.toList

let splitInstructions instructions =
    let splitInstruction (dir, amount) =
        [ dir ] @ List.replicate (amount - 1) Same

    List.collect splitInstruction instructions

let instructions =
    System.IO.File.ReadAllText "input/01.txt" |> parseLine |> splitInstructions

let initialState =
    { position = 0, 0
      orientation = North
      seen = Set.empty }

let rec applyInstructions state instructions checkSeen =
    match instructions with
    | [] -> state.position
    | instruction :: instructions ->
        let newState = step state instruction

        if checkSeen && Set.contains newState.position state.seen then
            newState.position
        else
            applyInstructions newState instructions checkSeen

applyInstructions initialState instructions false
|> distance initialState.position
|> printfn "Part 1: %i"

applyInstructions initialState instructions true
|> distance initialState.position
|> printfn "Part 2: %i"
