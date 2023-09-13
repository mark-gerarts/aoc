module AoC2015.Day20

let getDivisors n filtered =
    let upperLimit = n |> float |> sqrt |> int

    let divisors =
        seq {
            for i in { 1..upperLimit } do
                match n % i = 0, filtered with
                | true, false ->
                    yield i
                    yield n / i
                | true, true when n / i <= 50 -> yield i
                | true, true when n / (n / i) <= 50 -> yield n / i
                | _, _ -> ()
        }

    Seq.distinct divisors

let solve filtered numPresents =
    Seq.initInfinite ((+) 1)
    |> Seq.map (fun i -> getDivisors i filtered |> Seq.sum |> (*) numPresents)
    |> Seq.findIndex ((<) 36000000)
    |> (+) 1

let run _ =
    solve false 10 |> printfn "Part 1: %d"
    solve true 11 |> printfn "Part 2: %d"
