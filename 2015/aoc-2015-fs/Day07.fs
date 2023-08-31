module AoC2015.Day07

open FParsec

type RawInput =
    | Signal of uint16
    | Wire of string

type Input =
    | Raw of RawInput
    | And of RawInput * RawInput
    | Or of RawInput * RawInput
    | LShift of RawInput * int
    | RShift of RawInput * int
    | Not of RawInput

type Output = string

type Instruction = Input * Output

// Some more FParsec learning.
let parseLine line =
    let pSignal = puint16 |>> Signal
    let pWireInput = regex "[a-z]+" |>> Wire
    let pRawInput = choice [ pSignal; pWireInput ]
    let pAnd = pipe2 pRawInput (pstring " AND " >>. pRawInput) (fun l r -> And(l, r))
    let pOr = pipe2 pRawInput (pstring " OR " >>. pRawInput) (fun l r -> Or(l, r))

    let pLShift =
        pipe2 pRawInput (pstring " LSHIFT " >>. pint32) (fun l r -> LShift(l, r))

    let pRShift =
        pipe2 pRawInput (pstring " RSHIFT " >>. pint32) (fun l r -> RShift(l, r))

    let pNot = pstring "NOT " >>. pRawInput |>> Not

    let pInput =
        List.map attempt [ pAnd; pOr; pLShift; pRShift; pNot; pRawInput |>> Raw ]
        |> choice

    let pOutput = regex "[a-z]+"
    let pInstruction = pipe2 pInput (pstring " -> " >>. pOutput) (fun i o -> (i, o))

    match run pInstruction line with
    | Success(result, _, _) -> result
    | _ -> failwithf "Parse error on line '%s'" line

let rec calculateSignals instructions knownValues =
    let rec getRaw x = x |> Raw |> getValue

    and getValue input =
        match input with
        | Raw(Signal signal) -> Some signal
        | Raw(Wire name) -> Map.tryFind name knownValues
        | Or(l, r) -> Option.map2 (|||) (getRaw l) (getRaw r)
        | And(l, r) -> Option.map2 (&&&) (getRaw l) (getRaw r)
        | LShift(x, amount) -> Option.map (fun x -> x <<< amount) (getRaw x)
        | RShift(x, amount) -> Option.map (fun x -> x >>> amount) (getRaw x)
        | Not(x) -> Option.map (~~~) (getRaw x)

    let updateIfKnown known (input, output) =
        match getValue input with
        | Some(value) -> Map.add output value known
        | None -> known

    let newKnown = instructions |> Seq.fold updateIfKnown knownValues

    if (Map.count newKnown) = (Map.count knownValues) then
        newKnown
    else
        calculateSignals instructions newKnown

let overrideB instructions newValue =
    let overrideIfMatch instruction =
        match instruction with
        | Raw(Signal _), "b" -> Raw(Signal newValue), "b"
        | i -> i

    instructions |> Seq.map overrideIfMatch

let run filename =
    let instructions =
        filename |> System.IO.File.ReadLines |> Seq.map parseLine |> Seq.toList

    let outputValues = calculateSignals instructions Map.empty
    let a = Map.find "a" outputValues

    printfn "Part 1: %i" a

    calculateSignals (overrideB instructions a) Map.empty
    |> Map.find "a"
    |> printfn "Part 2: %i"
