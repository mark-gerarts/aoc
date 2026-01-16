#r "nuget: FParsec"

open FParsec

type Instruction =
    | Rect of int * int
    | RotateRow of int * int
    | RotateCol of int * int

let parseLine line =
    let pRect = pstring "rect " >>. pint32 .>>. (pchar 'x' >>. pint32) |>> Rect

    let pRotateRow =
        pstring "rotate row y=" >>. pint32 .>>. (pstring " by " >>. pint32)
        |>> RotateRow

    let pRotateCol =
        pstring "rotate column x=" >>. pint32 .>>. (pstring " by " >>. pint32)
        |>> RotateCol

    let pInstruction = choice [ pRect; pRotateRow; pRotateCol ]

    match run pInstruction line with
    | Success(out, _, _) -> out
    | _ -> failwithf "Unable to parse instruction %s" line

let width, height = 50, 6

let grid = Array.init (width * height) (fun _ -> false)

let posToIndex (x, y) = y * width + x

let indexToPos i = i % width, i / width

let printGrid grid =
    Array.iteri
        (fun i v ->
            if i % width = 0 then
                do printfn ""

            if v then printf "#" else printf ".")
        grid

    printfn ""


let applyInstruction grid instruction =
    match instruction with
    | Rect(w, h) ->
        Array.mapi
            (fun i v ->
                let x, y = indexToPos i
                if x < w && y < h then true else v)
            grid
    | RotateRow(row, amount) ->
        let rotateOne (grid: bool array) =
            Array.mapi
                (fun i v ->
                    let x, y = indexToPos i

                    if y = row then
                        let prevX = if x = 0 then width - 1 else x - 1
                        grid[posToIndex (prevX, y)]
                    else
                        v)
                grid

        seq { 0 .. amount - 1 } |> Seq.fold (fun grid _ -> rotateOne grid) grid
    | RotateCol(col, amount) ->
        let rotateOne (grid: bool array) =
            Array.mapi
                (fun i v ->
                    let x, y = indexToPos i

                    if x = col then
                        let prevY = if y = 0 then height - 1 else y - 1
                        grid[posToIndex (x, prevY)]
                    else
                        v)
                grid

        seq { 0 .. amount - 1 } |> Seq.fold (fun grid _ -> rotateOne grid) grid

let output =
    System.IO.File.ReadLines "input/08.txt"
    |> Seq.map parseLine
    |> Seq.fold applyInstruction grid

output |> Array.filter id |> Array.length |> printfn "Part 1: %i"

printGrid output
