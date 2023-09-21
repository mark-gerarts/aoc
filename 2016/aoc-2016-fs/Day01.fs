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

type State = Pos * Direction

let changeDirection currentDir action =
    let directions = [| North; East; South; West |]
    let currentIndex = Array.findIndex ((=) currentDir) directions

    let nextIndex =
        match action with
        | Left _ -> currentIndex - 1
        | Right _ -> currentIndex + 1

    directions[nextIndex % 4]

let amount action =
    match action with
    | Left amount -> amount
    | Right amount -> amount

let move ((x, y), dir) amount =
    match dir with
    | North -> (x, y + amount)
    | South -> (x, y - amount)
    | East -> (x + amount, y)
    | West -> (x - amount, y)

let applyAction (pos, dir) action =
    let newDir = changeDirection dir action
    let newPos = move (pos, newDir) (amount action)
    (newPos, newDir)

let run filename = printfn "Day 01"
