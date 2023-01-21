module AoC2022.Day11

open System.Text.RegularExpressions

type Monkey =
    { id: int
      items: list<bigint>
      operation: bigint -> bigint
      divisibleBy: bigint
      ifTrue: int
      ifFalse: int
      inspectCount: int }

let parseMonkey (monkeyInput: string) =
    let lines = monkeyInput.Split('\n')

    if Array.length lines <> 6 then
        failwith "Missing input line"

    let id =
        match Regex("\d+").Match lines[0] with
        | m when m.Success -> int m.Value
        | _ -> failwith "Unable to parse ID"

    let startingItems =
        match Regex("\d+").Matches lines[1] with
        | m when m.Count > 0 -> m |> Seq.map (fun m -> bigint (int m.Value)) |> Seq.toList
        | _ -> failwith $"Monkey {id} is not holding any items"

    let operation =
        let formula = (lines[ 2 ].Split('=')[1]).Trim()
        let parts = formula.Split(' ')

        match parts[1], parts[2] with
        | "+", "old" -> fun old -> old + old
        | "*", "old" -> fun old -> old * old
        | "+", number -> fun old -> old + bigint (int number)
        | "*", number -> fun old -> old * bigint (int number)
        | _ -> failwith $"Unable to parse formula for {id}"

    let divisibleBy =
        match Regex("\d+").Match lines[3] with
        | m when m.Success -> bigint (int m.Value)
        | _ -> failwith $"Unable to parse test for {id}"

    let ifTrue =
        match Regex("\d+").Match lines[4] with
        | m when m.Success -> int m.Value
        | _ -> failwith $"Unable to parse ifTrue for {id}"

    let ifFalse =
        match Regex("\d+").Match lines[5] with
        | m when m.Success -> int m.Value
        | _ -> failwith $"Unable to parse ifFalse for {id}"

    { id = id
      items = startingItems
      operation = operation
      divisibleBy = divisibleBy
      ifTrue = ifTrue
      ifFalse = ifFalse
      inspectCount = 0 }

let addItem monkey item =
    { monkey with items = List.append monkey.items [ item ] }

let takeItem monkey =
    match monkey.items with
    | [] -> monkey, None
    | x :: xs -> { monkey with items = xs }, Some x

let rec throwTo item id monkeys =
    match monkeys with
    | [] -> monkeys
    | x :: xs when x.id = id -> addItem x item :: xs
    | x :: xs -> x :: throwTo item id xs

let rec replaceMonkey monkey monkeys =
    match monkeys with
    | [] -> []
    | x :: xs when x.id = monkey.id -> monkey :: xs
    | x :: xs -> x :: replaceMonkey monkey xs

let rec findMonkey monkeys id =
    List.tryFind (fun m -> m.id = id) monkeys

let rec playTurn reliefOp monkey monkeys =
    match takeItem monkey with
    | _, None -> monkeys
    | updatedMonkey, Some item ->
        let updatedItem = item |> monkey.operation |> reliefOp

        let testResult = updatedItem % monkey.divisibleBy = bigint 0
        let targetMonkey = if testResult then monkey.ifTrue else monkey.ifFalse

        let updatedMonkey' =
            { updatedMonkey with inspectCount = updatedMonkey.inspectCount + 1 }

        let updatedMonkeys =
            throwTo updatedItem targetMonkey monkeys |> replaceMonkey updatedMonkey'

        playTurn reliefOp updatedMonkey' updatedMonkeys

let playRound reliefOp monkeys =
    let rec go monkeys i =
        match findMonkey monkeys i with
        | None -> monkeys
        | Some monkey ->
            let updatedMonkeys = playTurn reliefOp monkey monkeys
            go updatedMonkeys (i + 1)

    go monkeys 0

let parseInput =
    System.IO.File.ReadAllText("./input/11.txt").Trim()
    |> Regex("\n\n").Split
    |> Seq.map parseMonkey
    |> Seq.toList

let playRounds n decreaseWorry =
    let mutable monkeys = parseInput

    let reliefOp =
        if decreaseWorry then
            fun (i: bigint) -> i / bigint 3
        else
            let commonMult =
                monkeys |> List.map (fun m -> m.divisibleBy) |> List.reduce (fun a b -> a * b)

            fun (i: bigint) -> i % commonMult

    for _ in { 1..n } do
        monkeys <- playRound reliefOp monkeys

    monkeys
    |> List.map (fun m -> m.inspectCount)
    |> List.sortDescending
    |> List.take 2
    |> List.map bigint
    |> List.reduce (fun a b -> a * b)

let run =
    printfn "Part A: %A" (playRounds 20 true)
    printfn "Part B: %A" (playRounds 10000 false)
