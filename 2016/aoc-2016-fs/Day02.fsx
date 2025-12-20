let input =
    let parseDirection char =
        match char with
        | 'L' -> -1, 0
        | 'R' -> 1, 0
        | 'U' -> 0, -1
        | 'D' -> 0, 1
        | _ -> failwithf "Invalid input char %c" char

    System.IO.File.ReadLines "input/02.txt" |> Seq.map (Seq.map parseDirection)

let keypad1 = array2D [ [ "1"; "2"; "3" ]; [ "4"; "5"; "6" ]; [ "7"; "8"; "9" ] ]

let keypad2 =
    array2D
        [ [ ""; ""; "1"; ""; "" ]
          [ ""; "2"; "3"; "4"; "" ]
          [ "5"; "6"; "7"; "8"; "9" ]
          [ ""; "A"; "B"; "C"; "" ]
          [ ""; ""; "D"; ""; "" ] ]

let tryItem array2d (x, y) =
    if x < 0 || x >= Array2D.length1 array2d || y < 0 || y >= Array2D.length2 array2d then
        None
    else
        Some array2d[y, x]

let step keypad (x, y) (dx, dy) =
    let newX, newY = x + dx, y + dy

    match tryItem keypad (newX, newY) with
    | Some "" -> x, y
    | Some _ -> newX, newY
    | None -> x, y

let solve keypad start =
    input
    |> Seq.fold (fun s line -> Seq.fold (step keypad) (List.head s) line :: s) [ start ]
    |> List.rev
    |> List.tail
    |> List.choose (tryItem keypad)
    |> List.map string
    |> System.String.Concat

solve keypad1 (1, 1) |> printfn "Part 1: %s"
solve keypad2 (0, 2) |> printfn "Part 2: %s"
