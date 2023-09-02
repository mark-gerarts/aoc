module AoC2015.Day11

let increment password =
    let rec go xs =
        match xs with
        | [] -> failwith "Overflow"
        | x :: xs when x < 25 -> (x + 1) :: xs
        | _ :: xs -> 0 :: go xs

    List.rev password |> go |> List.rev

let isValid password =
    let containsIncreasing password =
        let isIncreasing window =
            match window with
            | [ a; b; c ] -> (a + 1) = b && (b + 1) = c
            | _ -> false

        password |> List.windowed 3 |> List.exists isIncreasing

    let containsForbidden password =
        [ 'i'; 'l'; 'o' ]
        |> List.map (fun c -> int c - int 'a')
        |> List.exists (fun i -> List.contains i password)

    let containsPairs password =
        let rec go password pair1 =
            match password, pair1 with
            | x :: y :: xs, None when x = y -> go xs (Some x)
            | x :: y :: _, Some a when x = y && x <> a -> true
            | _ :: xs, pair1 -> go xs pair1
            | [], _ -> false

        go password None

    let conditions = [| containsIncreasing; not << containsForbidden; containsPairs |]

    conditions |> Array.forall (fun c -> c password)

let toDigits password =
    password |> Seq.map (fun c -> int c - int 'a') |> Seq.toList

let toString password =
    password
    |> Seq.map ((+) (int 'a'))
    |> Seq.map char
    |> Seq.toArray
    |> System.String

let rec solve sequence =
    match isValid sequence with
    | true -> sequence
    | false -> sequence |> increment |> solve

let run filename =
    let sequence = filename |> System.IO.File.ReadLines |> Seq.head |> toDigits
    let newSequence = solve sequence

    newSequence |> toString |> printfn "Part 1: %s"
    newSequence |> increment |> solve |> toString |> printfn "Part 2: %s"
