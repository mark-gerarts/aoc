module AoC2015.Day06

open FParsec

type Action =
    | TurnOn
    | TurnOff
    | Toggle

type Point = int * int

type Area = Point * Point

type Instruction = Action * Area

type Grid = Set<Point>

let parseLine (line: string) : Instruction =
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

let applyAction action grid point =
    match action with
    | TurnOn -> Set.add point grid
    | TurnOff -> Set.remove point grid
    | Toggle when Set.contains point grid -> Set.remove point grid
    | Toggle -> Set.add point grid

let getPointsForArea ((x1, y1), (x2, y2)) =
    seq {
        for x in x1..x2 do
            for y in y1..y2 do
                yield (x, y)
    }

let applyInstruction grid instruction =
    let (action, area) = instruction

    area |> getPointsForArea |> Seq.fold (applyAction action) grid

let run filename =
    filename
    |> System.IO.File.ReadLines
    |> Seq.map parseLine
    |> Seq.fold applyInstruction Set.empty
    |> Set.count
    |> printfn "Part 1: %i"
