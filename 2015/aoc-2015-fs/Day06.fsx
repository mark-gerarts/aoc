#r "nuget: FParsec"

open FParsec

type Action =
    | TurnOn
    | TurnOff
    | Toggle

// Using native Sets/Maps is super slow, so we resort to a mutable array
// wrapped in a class instead.
type Grid(actionHandler: Action -> int -> int) =
    let mutable grid = Array.zeroCreate (1000 * 1000)

    member _.applyInstruction(action, ((x1, y1), (x2, y2))) =
        for x in x1..x2 do
            for y in y1..y2 do
                let i = y * 1000 + x
                grid[i] <- (actionHandler action) grid[i]

    member _.brightness = grid |> Array.sum

// Some overkill FParsec parsing
let parseLine (line: string) =
    let pCoord = tuple2 pint32 (pchar ',' >>. pint32)
    let pArea = tuple2 pCoord (pstring " through " >>. pCoord)
    let pTurnOn = pstring "turn on" >>% TurnOn
    let pTurnOff = pstring "turn off" >>% TurnOff
    let pToggle = pstring "toggle" >>% Toggle
    let pAction = choice [ pTurnOn; pTurnOff; pToggle ]
    let pInstruction = tuple2 pAction (spaces >>. pArea)

    match run pInstruction line with
    | Success(instruction, _, _) -> instruction
    | _ -> failwithf "Failed to parse line '%s'" line

let partAHandler action oldValue =
    match action with
    | TurnOn -> 1
    | TurnOff -> 0
    | Toggle -> if oldValue = 1 then 0 else 1

let partBHandler action oldValue =
    match action with
    | TurnOn -> oldValue + 1
    | TurnOff -> if oldValue = 0 then 0 else oldValue - 1
    | Toggle -> oldValue + 2

let instructions = System.IO.File.ReadLines "input/06.txt" |> Seq.map parseLine

let gridA = new Grid(partAHandler)
instructions |> Seq.iter gridA.applyInstruction
gridA.brightness |> printfn "Part 1: %i"

let gridB = new Grid(partBHandler)
instructions |> Seq.iter gridB.applyInstruction
gridB.brightness |> printfn "Part 2: %i"
