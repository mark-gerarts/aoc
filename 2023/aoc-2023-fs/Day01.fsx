#r "nuget: FsUnit,6.0.0"

open System.Text.RegularExpressions
open FsUnit

let getCalibrationValue string =
    let digits = Regex.Matches(string, "\d") |> Seq.map (_.Value >> int)
    let firstDigit = Seq.head digits
    let lastDigit = Seq.last digits

    firstDigit * 10 + lastDigit

let digitize (string: string) =
    string
    |> _.Replace("one", "o1e")
    |> _.Replace("two", "t2o")
    |> _.Replace("three", "t3e")
    |> _.Replace("four", "f4r")
    |> _.Replace("five", "f5e")
    |> _.Replace("six", "s6x")
    |> _.Replace("seven", "s7n")
    |> _.Replace("eight", "e8t")
    |> _.Replace("nine", "n9e")

let input = System.IO.File.ReadLines "input/01.txt"
input |> Seq.sumBy getCalibrationValue |> printfn "Part 1: %i"
input |> Seq.sumBy (digitize >> getCalibrationValue) |> printfn "Part 2: %i"

"eightree45" |> getCalibrationValue |> should equal 45
"eighthree" |> digitize |> getCalibrationValue |> should equal 83
