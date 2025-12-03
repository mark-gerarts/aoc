#r "nuget: FsUnit,7.1.1"

open FsUnit

let parseLine = Seq.map (string >> int64) >> Seq.toList

let parseInput = "input/03.txt" |> System.IO.File.ReadLines |> Seq.map parseLine

let listToInt = List.rev >> List.mapi (fun i x -> x * pown 10L i) >> List.sum

let rec largestSubsequence length xs =
    let rec candidateDigits xs =
        match xs with
        | x :: xs' when List.length xs >= length -> (x, xs') :: candidateDigits xs'
        | _ -> []

    if length = 0 then
        []
    else
        let largestDigit, tail = candidateDigits xs |> List.maxBy fst

        largestDigit :: largestSubsequence (length - 1) tail

let maxJoltage numBatteries bank =
    largestSubsequence numBatteries bank |> listToInt

parseInput |> Seq.sumBy (maxJoltage 2) |> printfn "Part 1: %i"
parseInput |> Seq.sumBy (maxJoltage 12) |> printfn "Part 2: %i"

"987654321111111" |> parseLine |> maxJoltage 2 |> should equal 98
"811111111111119" |> parseLine |> maxJoltage 2 |> should equal 89
"234234234234278" |> parseLine |> maxJoltage 2 |> should equal 78
"818181911112111" |> parseLine |> maxJoltage 2 |> should equal 92

"987654321111111" |> parseLine |> maxJoltage 12 |> should equal 987654321111L
"811111111111119" |> parseLine |> maxJoltage 12 |> should equal 811111111119L
"234234234234278" |> parseLine |> maxJoltage 12 |> should equal 434234234278L
"818181911112111" |> parseLine |> maxJoltage 12 |> should equal 888911112111L
