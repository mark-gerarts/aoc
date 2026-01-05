let leastAndMostFrequentElement xs =
    let sorted = xs |> Seq.countBy id |> Seq.sortBy snd |> Seq.map fst

    Seq.head sorted, Seq.last sorted

let chars =
    System.IO.File.ReadAllLines "input/06.txt"
    |> Seq.transpose
    |> Seq.map leastAndMostFrequentElement

chars |> Seq.map snd |> System.String.Concat |> printfn "Part 1: %s"
chars |> Seq.map fst |> System.String.Concat |> printfn "Part 2: %s"
