module AoC2022.Day05

open System.Text.RegularExpressions

type Stack = list<char>

let parseStacks (setup: string) =
    let stacks = Array.init 9 (fun _ -> [])
    let lines = setup.Split('\n')

    for line in lines do
        for i in [ 1 .. line.Length - 1 ] do
            if Regex("[A-Z]").IsMatch(line[ i ].ToString()) then
                let stackNumber = (i - 1) / 4
                stacks.[stackNumber] <- line[i] :: stacks.[stackNumber]

    stacks |> Seq.map List.rev |> Seq.toArray

let parseMove move =
    match Regex("\d+").Matches(move) |> Seq.toList with
    | [ amount; from; to' ] -> (int amount.Value, int from.Value - 1, int to'.Value - 1)
    | _ -> failwith "Could not parse move"

let applyMovesPartA (stacks: array<Stack>) moves =
    let stacks' = Array.copy stacks

    for (amount, from, to') in moves do
        for _ in 1..amount do
            let crate = List.head stacks'[from]
            stacks'[from] <- List.tail stacks'[from]
            stacks'[to'] <- crate :: stacks'[to']

    stacks'

let applyMovesPartB (stacks: array<Stack>) moves =
    let stacks' = Array.copy stacks

    for (amount, from, to') in moves do
        let crates = List.take amount stacks'[from]
        stacks'[from] <- List.skip amount stacks'[from]
        stacks'[to'] <- List.append crates stacks'[to']

    stacks'

let readTopCrates (stacks: array<Stack>) =
    stacks |> Seq.map List.head |> Seq.map string |> String.concat ""

let run =
    let input = System.IO.File.ReadAllText("./input/05.txt").TrimEnd()
    let parts = Regex("\n\n").Split(input)
    let stacks = parts[0] |> parseStacks
    let moves = parts[ 1 ].Split('\n') |> Seq.map parseMove

    printfn "Part A: %s" <| (applyMovesPartA stacks moves |> readTopCrates)
    printfn "Part B: %s" <| (applyMovesPartB stacks moves |> readTopCrates)
