module AoC2015.Day06

open FParsec

type Action =
    | TurnOn
    | TurnOff
    | Toggle

// Using native Sets/Maps is super slow, so we resort to a mutable array
// wrapped in a class instead.
type Grid(actionHandler: Action -> int -> int) =
    let mutable grid = Array.zeroCreate (1000 * 1000)

    member private self.updateValueAt (x, y) fn =
        let index = 1000 * y + x
        grid[index] <- fn grid[index]

    member self.handleAction action p =
        self.updateValueAt p (actionHandler action)

    member self.getBrightness() = Array.sum grid

// Some overkill FParsec parsing
let parseLine (line: string) =
    let toTuple a b = (a, b)
    let pCoord = pipe2 pint32 (pchar ',' >>. pint32) toTuple
    let pArea = pipe2 pCoord (pstring " through " >>. pCoord) toTuple
    let pTurnOn = pstring "turn on" >>% TurnOn
    let pTurnOff = pstring "turn off" >>% TurnOff
    let pToggle = pstring "toggle" >>% Toggle
    let pAction = choice [ pTurnOn; pTurnOff; pToggle ]
    let pInstruction = pipe2 pAction (spaces >>. pArea) toTuple

    match run pInstruction line with
    | Success(instruction, _, _) -> instruction
    | _ -> failwithf "Failed to parse line '%s'" line

let getPointsForArea ((x1, y1), (x2, y2)) =
    seq {
        for x in x1..x2 do
            for y in y1..y2 do
                yield (x, y)
    }

let applyInstruction (grid: Grid) instruction =
    let (action, area) = instruction

    area |> getPointsForArea |> Seq.iter (grid.handleAction action)

let run filename =
    let partAHandler action oldValue =
        match action with
        | TurnOn -> 1
        | TurnOff -> 0
        | Toggle -> if oldValue = 1 then 0 else 1

    let gridA = new Grid(partAHandler)

    let partBHandler action oldValue =
        match action with
        | TurnOn -> oldValue + 1
        | TurnOff -> if oldValue = 0 then 0 else oldValue - 1
        | Toggle -> oldValue + 2

    let gridB = new Grid(partBHandler)

    let instructions = filename |> System.IO.File.ReadLines |> Seq.map parseLine
    instructions |> Seq.iter (applyInstruction gridA)
    instructions |> Seq.iter (applyInstruction gridB)

    printfn "Part 1: %i" <| gridA.getBrightness ()
    printfn "Part 2: %i" <| gridB.getBrightness ()
