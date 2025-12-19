let getDivisors n filtered =
    let upperLimit = n |> float |> sqrt |> int

    seq {
        for i in { 1..upperLimit } do
            if n % i = 0 then
                if not filtered then
                    yield i
                    yield n / i
                elif n / i <= 50 then
                    yield i
                elif n / (n / i) <= 50 then
                    yield n / i
    }
    |> Seq.distinct

let puzzleInput = System.IO.File.ReadAllText "input/20.txt" |> _.Trim() |> int

let solve filtered numPresents =
    Seq.initInfinite (fun i -> getDivisors i filtered |> Seq.sum |> (*) numPresents)
    |> Seq.findIndex ((<) puzzleInput)

solve false 10 |> printfn "Part 1: %d"
solve true 11 |> printfn "Part 2: %d"
