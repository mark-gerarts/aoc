let parseInput =
    "input/03.txt"
    |> System.IO.File.ReadLines
    |> Seq.map (Seq.map (string >> int) >> Seq.toList)

// Finds all permutations where the order of the original input list is
// preserved. E.g. [1; 2; 3] => [1; 2] [1; 3] [2; 3]
let rec consecutivePermutationsOfLength length xs =
    seq {
        if length = 1 then
            yield! List.map List.singleton xs
        elif length > List.length xs then
            ()
        else
            let head, tail = List.head xs, List.tail xs

            yield! consecutivePermutationsOfLength length tail

            for t in consecutivePermutationsOfLength (length - 1) tail do
                yield head :: t
    }


let listToInt xs =
    xs |> List.rev |> List.mapi (fun i x -> x * pown 10 i) |> List.sum


let maxJoltage battery =
    consecutivePermutationsOfLength 2 battery |> Seq.map listToInt |> Seq.max

// TODO: this is okayish for n=2, but explodes for n=12 (ofcourse).
parseInput |> Seq.sumBy maxJoltage |> printfn "%A"
