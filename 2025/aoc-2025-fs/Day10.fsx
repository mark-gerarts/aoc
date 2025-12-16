#r "nuget: FParsec,1.1.1"
#r "nuget: Flips"

open Flips
open Flips.Types
open FParsec

type State =
    | On
    | Off

let flip state = if state = On then Off else On

let parseLine line =
    let pOn = pchar '#' >>% On
    let pOff = pchar '.' >>% Off
    let pDesiredState = pchar '[' >>. many (choice [ pOn; pOff ]) .>> pstring "] "

    let pWiring = pchar '(' >>. sepBy pint32 (pchar ',') .>> pchar ')'
    let pWirings = many (pWiring .>> pchar ' ')

    let pJoltages = pchar '{' >>. sepBy pint32 (pchar ',') .>> pchar '}'

    let parseDiagram = tuple3 pDesiredState pWirings pJoltages

    match run parseDiagram line with
    | Success(x, _, _) -> x
    | _ -> failwithf "Could not parse line '%s'" line

let rec applyWiring wiring state =
    match wiring with
    | [] -> state
    | i :: is -> List.updateAt i (flip (List.item i state)) state |> applyWiring is

let turnAllOff desiredState =
    List.replicate (List.length desiredState) Off

let numStepsNeededPart1 (desiredState, wirings, _) =
    let rec go seenStates currentStates stepCount =
        let newStates =
            seq {
                for state in currentStates do
                    for wiring in wirings do
                        applyWiring wiring state
            }
            |> Seq.filter (fun s -> not <| Set.contains s seenStates)
            |> Set.ofSeq

        if Set.contains desiredState newStates then
            stepCount + 1
        else
            go (Set.union seenStates newStates) newStates (stepCount + 1)

    let initial = turnAllOff desiredState

    go (Set.singleton initial) (Set.singleton initial) 0

// Model the problem as a set of linear equations, which we can pass to a
// solver. E.g. for the line with buttons [0]; [1]; [0;1] and target [2;4], we
// create the following equations:
//
// - b0 + b2 = 2
// - b1 + b2 = 4
// - total = b0 + b1
//
// where bi is the number of times button i is pressed. Then we solve while
// minimizing `total`.
let numStepsNeededPart2 (_, wirings, joltageLevels) =
    let buttonExpressions =
        wirings
        |> Seq.indexed
        |> Seq.map (fun (i, targets) ->
            Decision.createInteger (sprintf "button%i" i) 0 infinity * 1.0, Set.ofList targets)

    let wiringConstraints =
        seq {
            for target, count in Seq.indexed joltageLevels do
                let relevantButtons =
                    buttonExpressions |> Seq.filter (snd >> Set.contains target) |> Seq.map fst

                Constraint.create (sprintf "Target%i" target) (Seq.sum relevantButtons == float count)
        }

    let total = buttonExpressions |> Seq.sumBy fst
    let objective = Objective.create "Total presses" Minimize total

    let model'' = Model.create objective |> Model.addConstraints wiringConstraints

    match Solver.solve Settings.basic model'' with
    | Optimal solution -> Objective.evaluate solution objective |> int
    | _ -> failwithf "Unable to solve %A %A" wirings joltageLevels

let lines = System.IO.File.ReadLines "input/10.txt" |> Seq.map parseLine

lines |> Seq.sumBy numStepsNeededPart1 |> printfn "Part 1: %i"
lines |> Seq.sumBy numStepsNeededPart2 |> printfn "Part 2: %i"
