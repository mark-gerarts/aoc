module AoC2022.Day06

let rec solve windowSize input =
    let isDistinct window =
        windowSize = (Seq.distinct window |> Seq.length)

    let matchAtIndex =
        Seq.windowed windowSize input |> Seq.takeWhile (not << isDistinct) |> Seq.length

    matchAtIndex + windowSize

let run =
    let input = System.IO.File.ReadAllText("./input/06.txt")

    printfn "Part A: %i" <| solve 4 input
    printfn "Part B: %i" <| solve 14 input
