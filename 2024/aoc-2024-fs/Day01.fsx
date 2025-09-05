let parseInput filename =
    let pairs = filename |> System.IO.File.ReadAllLines |> Seq.map _.Split("   ")

    let lefts = pairs |> Seq.map (fun pair -> pair[0]) |> Seq.map int
    let rights = pairs |> Seq.map (fun pair -> pair[1]) |> Seq.map int

    lefts, rights

let part1 (lefts, rights) =
    Seq.zip (Seq.sort lefts) (Seq.sort rights)
    |> Seq.sumBy (fun (l, r) -> abs (l - r))

let part2 (lefts, rights) =
    lefts |> Seq.sumBy (fun l -> l * (Seq.filter ((=) l) rights |> Seq.length))

let input = parseInput "input/01.txt"
input |> part1 |> printfn "Part 1: %i"
input |> part2 |> printfn "Part 2: %i"
