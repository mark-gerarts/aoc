let parse filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map (fun l -> l.Split(' ') |> Seq.map int)

let isSafe line =
    let pairs = Seq.zip line (Seq.tail line)

    let allIncrement = pairs |> Seq.forall (fun (a, b) -> a < b)
    let allDecrement = pairs |> Seq.forall (fun (a, b) -> a > b)
    let allClose = pairs |> Seq.forall (fun (a, b) -> abs (a - b) <= 3)

    (allIncrement || allDecrement) && allClose

let part1 input =
    input |> Seq.filter isSafe |> Seq.length

let part2 input =
    let isSafeWithDampener line =
        line |> Seq.mapi (fun i _ -> Seq.removeAt i line |> isSafe) |> Seq.contains true

    input |> Seq.filter isSafeWithDampener |> Seq.length

let input = parse "input/02.txt"
input |> part1 |> printfn "Part 1: %i"
input |> part2 |> printfn "Part 2: %i"
