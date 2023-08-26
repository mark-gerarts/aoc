module AoC2015.Day02

type Box = int * int * int

let paperNeeded (l, w, h) =
    let lw = l * w
    let wh = w * h
    let hl = h * l
    let smallestSide = Array.min [| lw; wh; hl |]
    let surfaceArea = 2 * lw + 2 * wh + 2 * hl

    surfaceArea + smallestSide

let ribbonNeeded (l, w, h) =
    let perimeter = [| l; w; h |] |> Array.sort |> Array.take 2 |> Array.sum
    let volume = w * h * l

    2 * perimeter + volume

let parseLine (line: string) =
    match line.Split('x') with
    | [| l; w; h |] -> (int l, int w, int h)
    | _ -> failwithf "Could not parse %s" line

let solve file fn =
    System.IO.File.ReadLines(file) |> Seq.map parseLine |> Seq.sumBy fn

let run file =
    printfn "Part 1: %i" <| solve file paperNeeded
    printfn "Part 2: %i" <| solve file ribbonNeeded
