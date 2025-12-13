#r "nuget: FParsec,1.1.1"

#nowarn "57" // For Array.Parallel being experimental right now.

open FParsec
open System.Collections

type Toggle =
    | On
    | Off

type State = Toggle list

type Wiring = int list

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

// TODO: do we check if an old state returns (is this even correct)? Or if the
// initial state returns?
let takeStep (wirings: Wiring list) (searchSpace: (State list) list) : (State list) list =
    seq {
        for wiring in wirings do
            for stateHistory in searchSpace do
                let newState = applyWiring wiring (List.head stateHistory)
                let seen = List.contains newState stateHistory
                let isInitial = List.forall ((=) Off) newState

                if not seen && not isInitial then
                    newState :: stateHistory
    }
    |> Seq.toList

let numStepsNeeded (desiredState, wirings, _) =
    let rec go searchSpace count =
        if count > 5 then
            searchSpace |> Seq.collect id |> Seq.length |> printfn "%A"
            failwith "Too big :("

        let newSearchSpace = takeStep wirings searchSpace

        if newSearchSpace |> List.map List.head |> List.exists ((=) desiredState) then
            count + 1
        else
            go newSearchSpace (count + 1)

    let initialState = turnAllOff desiredState

    let initialSearchSpace =
        List.replicate (List.length wirings) initialState |> List.map List.singleton

    go initialSearchSpace 0

// TODO: memory jumps too high. Can we prune the search space more? Can we use
// more efficient data structures?
"[#.....#.##] (0,1,2,3,4,5) (0,1,3,5,6,7,8,9) (0,8,9) (0,2,3,5,6,7,8) (3,4,6,7,8,9) (2,3,6,9) (2,8) (0,1,2,3,4,5,6,7,9) (4,9) (2,7,9) {22,9,37,35,19,21,34,26,21,38}"
|> parseLine
|> numStepsNeeded
|> printfn "%A"

(*
System.IO.File.ReadLines "input/10.txt"
|> Seq.map parseLine
//|> Seq.toArray
//|> Array.Parallel.map numStepsNeeded
|> Seq.map numStepsNeeded
|> Seq.indexed
|> Seq.iter (printfn "%A")
*)
