module AoC2022.Day13

open System
open System.Text.RegularExpressions

type ParseResult =
    | OpenBracket
    | CloseBracket
    | Number of int

type CompareResult =
    | Left
    | Right

let singleton x = [ OpenBracket; Number x; CloseBracket ]

let rec parse input =
    match Seq.tryHead input with
    | None -> []
    | Some '[' -> OpenBracket :: parse (Seq.tail input)
    | Some ']' -> CloseBracket :: parse (Seq.tail input)
    | Some x when Char.IsDigit x ->
        let stringInput = Seq.takeWhile Char.IsDigit input
        let tail = Seq.skipWhile Char.IsDigit input
        let number = int (String.Concat stringInput)

        Number number :: parse tail
    | Some ',' -> parse (Seq.tail input)
    | Some x -> failwith $"Unexpected character: {x}"

let parsePair (input: string) =
    let pair = input.Split('\n')

    if pair.Length <> 2 then
        do failwith "Invalid input"

    parse pair[0], parse pair[1]

// Returns Left if the left value is higher, idem for Right.
let rec compare left right =
    match List.tryHead left, List.tryHead right with
    | None, _ -> Right
    | _, None -> Left
    | Some l, Some r ->
        let ltail = List.tail left
        let rtail = List.tail right

        match l, r with
        | Number a, Number b when a < b -> Right
        | Number a, Number b when a > b -> Left
        | Number a, Number b when a = b -> compare ltail rtail
        | Number a, OpenBracket -> compare (List.append (singleton a) ltail) right
        | OpenBracket, Number b -> compare left (List.append (singleton b) rtail)
        | OpenBracket, OpenBracket -> compare ltail rtail
        | CloseBracket, CloseBracket -> compare ltail rtail
        | CloseBracket, _ -> Right
        | _, CloseBracket -> Left
        | _, _ -> failwith "Unmatched brackets :("

let partA =
    System.IO.File.ReadAllText("./input/13.txt").Trim()
    |> Regex("\n\n").Split
    |> Seq.map parsePair
    |> Seq.map (fun (l, r) -> compare l r)
    |> Seq.zip (Seq.initInfinite (fun i -> i + 1))
    |> Seq.filter (fun (_, result) -> result = Right)
    |> Seq.sumBy fst

let partB =
    let input =
        System.IO.File.ReadAllLines("./input/13.txt")
        |> Seq.filter (not << String.IsNullOrWhiteSpace)
        |> Seq.map parse
        |> Seq.toList

    let divider1 = [ OpenBracket; OpenBracket; Number 2; CloseBracket; CloseBracket ]
    let divider2 = [ OpenBracket; OpenBracket; Number 6; CloseBracket; CloseBracket ]

    List.append [ divider1; divider2 ] input
    |> Seq.sortWith (fun a b -> if compare a b = Left then 1 else -1)
    |> Seq.zip (Seq.initInfinite (fun i -> i + 1))
    |> Seq.filter (fun (_, l) -> l = divider1 || l = divider2)
    |> Seq.map fst
    |> Seq.reduce (fun a b -> a * b)

let run =
    printfn "Part A: %i" partA
    printfn "Part B: %i" partB
