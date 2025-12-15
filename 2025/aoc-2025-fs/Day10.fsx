#r "nuget: FParsec,1.1.1"

#nowarn "57" // For Array.Parallel being experimental right now.

open FParsec

type Toggle =
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

let numStepsNeeded (desiredState, wirings, _) =
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


System.IO.File.ReadLines "input/10.txt"
|> Seq.sumBy (parseLine >> numStepsNeeded)
|> printfn "Part 1: %i"
