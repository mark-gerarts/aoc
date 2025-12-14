type Shape = bool array

let overlaps a b = Array.exists2 (&&) a b

let rotate a =
    let rotateMapping index =
        match index with
        | 0 -> 2
        | 1 -> 5
        | 2 -> 8
        | 3 -> 1
        | 4 -> 4
        | 5 -> 7
        | 6 -> 0
        | 7 -> 3
        | 8 -> 6
        | _ -> failwith "No 3x3 shape"

    Array.permute rotateMapping a

let flipH a =
    let flipMapping index =
        match index with
        | 0 -> 2
        | 1 -> 1
        | 2 -> 0
        | 3 -> 5
        | 4 -> 4
        | 5 -> 3
        | 6 -> 8
        | 7 -> 7
        | 8 -> 6
        | _ -> failwith "No 3x3 shape"

    Array.permute flipMapping a

let flipV a =
    let flipMapping index =
        match index with
        | 0 -> 6
        | 1 -> 7
        | 2 -> 8
        | 3 -> 3
        | 4 -> 4
        | 5 -> 5
        | 6 -> 0
        | 7 -> 1
        | 8 -> 2
        | _ -> failwith "No 3x3 shape"

    Array.permute flipMapping a

let images shape =
    let rotations shape =
        Seq.scan (fun s _ -> rotate s) shape { 0..3 }

    seq {
        yield! rotations shape
        yield! rotations (flipH shape)
        yield! rotations (flipV shape)
    }
    |> Seq.distinct


let rotations shape =
    Seq.scan (fun s _ -> rotate s) shape { 0..3 }

printfn "%A" (rotations [| true; true; true; false; false; false; false; false; false |])

let parseInput =
    let parseShapeSection (section: string) =
        section.Split "\n"
        |> Array.tail
        |> System.String.Concat
        |> Seq.map ((=) '#')
        |> Seq.toArray

    let parsePackingProblem (line: string) =
        match line.Split ": " with
        | [| size; amounts |] ->
            match size.Split "x" with
            | [| w; h |] -> (int w, int h), amounts.Split " " |> Array.map int
            | _ -> failwithf "Unable to parse wxh %s" size
        | _ -> failwithf "Unable to parse line %s" line

    let input = System.IO.File.ReadAllText "input/12.test" |> _.Trim()
    let sections = input.Split "\n\n"
    let shapeSections = sections[0 .. sections.Length - 2]
    let packingProblems = Array.last sections

    shapeSections |> Array.map parseShapeSection, packingProblems.Split "\n" |> Array.map parsePackingProblem

let shapes, packingProblems = parseInput

let place (w, h, grid) (x, y) piece =
    let posToIndex (x, y) = y * w + x

    if (x + 3) > w || (y + 3) > h then
        None
    else
        seq {
            for x in { x .. x + 3 } do
                for y in { y .. y + 3 } do
                    yield y * w + x
        }
        |> Seq.fold (fun g i -> if Array[i]









let fitsShapes packingProblem =
    let size, shapeCounts = packingProblem

    let shapes =
        shapeCounts
        |> Array.mapi (fun i count -> Array.replicate count shapes[i])
        |> Array.collect id

    shapes

printfn "%A" (fitsShapes packingProblems[0])
