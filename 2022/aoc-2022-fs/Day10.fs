module AoC2022.Day10

type Operation =
    { op: int -> int; cyclesRemaining: int }

type State =
    { operations: list<Operation>
      cycle: int
      x: int }

let addx y =
    { op = (fun x -> x + y)
      cyclesRemaining = 1 }

let noop = { op = id; cyclesRemaining = 0 }

let tick state =
    let applyOperation op =
        match op with
        | op when op.cyclesRemaining > 0 ->
            // Operation is not done yet.
            let updatedOperation = { op with cyclesRemaining = op.cyclesRemaining - 1 }

            { operations = updatedOperation :: List.tail state.operations
              cycle = state.cycle + 1
              x = state.x }
        | op ->
            // Operation is done, apply it.
            { operations = List.tail state.operations
              cycle = state.cycle + 1
              x = op.op state.x }

    match List.tryHead state.operations with
    | None -> state
    | Some operation -> applyOperation operation

let parseOperations =
    let parseOperation line =
        match line with
        | "noop" -> noop
        | _ -> addx (int (line.Split(' ')[1]))

    System.IO.File.ReadLines("./input/10.txt")
    |> Seq.map parseOperation
    |> Seq.toList

let partA state =
    let rec collectSignalStrengths state =
        if List.isEmpty state.operations then
            []
        else if List.contains state.cycle [ 20; 60; 100; 140; 180; 220 ] then
            printfn "Cycle: %i x: %i *: %i" state.cycle state.x (state.cycle * state.x)
            (state.cycle * state.x) :: collectSignalStrengths (tick state)
        else
            collectSignalStrengths (tick state)

    List.sum (collectSignalStrengths state)

let partB state =
    let rec collectPixels state =
        if List.isEmpty state.operations then
            []
        else
            let sprite = [ state.x - 1; state.x; state.x + 1 ]
            let overlaps = List.contains ((state.cycle - 1) % 40) sprite
            let pixel = if overlaps then "#" else "."

            pixel :: collectPixels (tick state)

    collectPixels state
    |> Seq.chunkBySize 40
    |> Seq.iter (fun row -> printfn "%s" (String.concat "" row))

let run =
    let initialState =
        { operations = parseOperations
          cycle = 1
          x = 1 }

    printfn "%A" <| partA initialState

    partB initialState
