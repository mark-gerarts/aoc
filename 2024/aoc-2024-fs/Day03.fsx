#r "nuget: FParsec,1.1.1"

open FParsec

type Instruction =
    | Mul of int * int
    | Do
    | Dont

let parseInput input =
    let pMul =
        pstring "mul(" >>. pint32 .>>. (pchar ',' >>. pint32 .>> pchar ')') |>> Mul

    let pDo = pstring "do()" >>% Do
    let pDont = pstring "don't()" >>% Dont

    let pInstruction = choice [ pMul; pDo; pDont ]
    let instructionOrJunk = attempt (pInstruction |>> Some) <|> (anyChar >>% None)

    let pInstructions = many instructionOrJunk |>> List.choose id

    match run pInstructions input with
    | Success(instructions, _, _) -> instructions
    | _ -> failwithf "Parse error; incorrect input"


let part1 instructions =
    instructions
    |> List.sumBy (function
        | Mul(a, b) -> a * b
        | _ -> 0)

let part2 instructions =
    let sumActiveMults (active, sum) instruction =
        match instruction with
        | Do -> true, sum
        | Dont -> false, sum
        | Mul(a, b) when active -> active, sum + a * b
        | _ -> active, sum

    instructions |> List.fold sumActiveMults (true, 0) |> snd

let input = "input/03.txt" |> System.IO.File.ReadAllText |> _.Trim() |> parseInput
input |> part1 |> printfn "Part 1: %i"
input |> part2 |> printfn "Part 2: %i"
