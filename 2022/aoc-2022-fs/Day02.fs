module AoC2022.Day02

type Shape =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Win
    | Lose
    | Draw

let scoreForShape shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreForOutcome outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let parseShape char =
    match char with
    | 'A' -> Rock
    | 'B' -> Paper
    | _ -> Scissors

let score myShape oppShape =
    let outcome =
        match myShape, oppShape with
        | Rock, Scissors -> Win
        | Rock, Paper -> Lose
        | Paper, Rock -> Win
        | Paper, Scissors -> Lose
        | Scissors, Paper -> Win
        | Scissors, Rock -> Lose
        | _ -> Draw

    scoreForOutcome outcome + scoreForShape myShape

let getScoreForLineA (line: string) : int =
    let oppShape = parseShape line[0]

    let myShape =
        match line[2] with
        | 'X' -> Rock
        | 'Y' -> Paper
        | _ -> Scissors

    score myShape oppShape

let getScoreForLineB (line: string) : int =
    let oppShape = parseShape line[0]

    let outcome =
        match line[2] with
        | 'X' -> Lose
        | 'Y' -> Draw
        | _ -> Win

    let myShape =
        match oppShape, outcome with
        | Rock, Win -> Paper
        | Rock, Lose -> Scissors
        | Paper, Win -> Scissors
        | Paper, Lose -> Rock
        | Scissors, Win -> Rock
        | Scissors, Lose -> Paper
        | _ -> oppShape

    score myShape oppShape

let solve (scoreFn: string -> int) =
    System.IO.File.ReadAllLines("./input/02.txt") |> Seq.sumBy scoreFn

let run =
    printfn "Part A: %i" (solve getScoreForLineA)
    printfn "Part B: %i" (solve getScoreForLineB)
