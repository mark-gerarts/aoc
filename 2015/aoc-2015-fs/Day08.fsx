open System.Text.RegularExpressions

let code2mem =
    let startOfString = "^\"", ""
    let endOfString = "\"$", ""
    let backslash = @"\\\\", @"\"
    let quote = @"\\\""", "\""
    let hexadecimal = @"\\x[0-9A-Fa-f]{2}", "x"

    [ startOfString; endOfString; backslash; quote; hexadecimal ]

let mem2code =
    let quote = "\"", "\\\""
    let backslash = @"\\", "\\\\"
    let startOrEnd = "^|$", "\""

    [ backslash; quote; startOrEnd ]

let transform line mapping =
    let replace line (pattern: string, replacement: string) =
        Regex(pattern).Replace(line, replacement)

    List.fold replace line mapping

let diff mapping line =
    abs <| String.length line - String.length (transform line mapping)

let input = System.IO.File.ReadLines "input/08.txt"

input |> Seq.sumBy (diff code2mem) |> printfn "Part 1: %i"
input |> Seq.sumBy (diff mem2code) |> printfn "Part 2: %i"
