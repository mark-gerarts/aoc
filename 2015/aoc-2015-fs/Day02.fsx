let paperNeeded (l, w, h) =
    let lw = l * w
    let wh = w * h
    let hl = h * l
    let smallestSide = List.min [ lw; wh; hl ]
    let surfaceArea = 2 * lw + 2 * wh + 2 * hl

    surfaceArea + smallestSide

let ribbonNeeded (l, w, h) =
    let perimeter = [ l; w; h ] |> List.sort |> List.take 2 |> List.sum
    let volume = w * h * l

    2 * perimeter + volume

let parseLine (line: string) =
    match line.Split 'x' with
    | [| l; w; h |] -> int l, int w, int h
    | _ -> failwithf "Could not parse %s" line

let input = System.IO.File.ReadLines "input/02.txt" |> Seq.map parseLine

input |> Seq.sumBy paperNeeded |> printfn "Part 1: %i"
input |> Seq.sumBy ribbonNeeded |> printfn "Part 2: %i"
