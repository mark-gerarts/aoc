module AoC2022.Day01

open System.Text.RegularExpressions

let calorieTotals =
    System.IO.File.ReadAllText("./input/01.txt").Trim()
    |> Regex("\n\n").Split
    |> Seq.map (fun group -> group |> Regex("\n").Split |> Seq.sumBy int)
    |> Seq.sortDescending

let partA = calorieTotals |> Seq.head

let partB = calorieTotals |> Seq.take 3 |> Seq.sum

let run =
    printfn "Part A: %i" partA
    printfn "Part B: %i" partB
