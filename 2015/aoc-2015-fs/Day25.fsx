open System.Text.RegularExpressions

let row, col =
    let input = System.IO.File.ReadAllText "input/25.txt"
    let matches = Regex("\d+").Matches input |> Seq.map (_.Value >> int)

    Seq.head matches, Seq.last matches

// Had to look this one up...
// Linear Congruential Generator
let firstCode = 20151125L
let nextCode curCode = curCode * 252533L % 33554393L

let codeCount row column =
    let n = row + column - 1
    n * (n - 1) / 2 + column

let count = codeCount row col

{ 2..count }
|> Seq.fold (fun acc _ -> nextCode acc) firstCode
|> printfn "Solution: %d"
