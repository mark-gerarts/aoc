module AoC2015.Day08

open System.Text.RegularExpressions

let code2mem =
    let startOfString = ("^\"", "")
    let endOfString = ("\"$", "")
    let backslash = (@"\\\\", @"\")
    let quote = (@"\\\""", "\"")
    let hexadecimal = (@"\\x[0-9A-Fa-f]{2}", "x")

    [| startOfString; endOfString; backslash; quote; hexadecimal |]

let mem2code =
    let quote = ("\"", "\\\"")
    let backslash = (@"\\", "\\\\")
    let startOrEnd = ("^|$", "\"")

    [| backslash; quote; startOrEnd |]

let transform line mapping =
    let folder line (pattern: string, replacement: string) =
        Regex(pattern).Replace(line, replacement)

    mapping |> Array.fold folder line

let diff mapping line =
    printfn "%s" line
    printfn "%s" (transform line mapping)
    printfn "---"
    String.length line - String.length (transform line mapping) |> abs

let run filename =
    let input = filename |> System.IO.File.ReadLines

    input |> Seq.map (diff code2mem) |> Seq.sum |> printfn "Part 1: %i"
    input |> Seq.map (diff mem2code) |> Seq.sum |> printfn "Part 2: %i"
