module AoC2015.Day15

open System.Collections.Generic
open FParsec

(*
    Very ugly solution, was not very awake for this one...
*)

type Ingredient =
    { capacity: int
      durability: int
      flavor: int
      texture: int
      calories: int }

// When given an integer, generates all possible ways it can be summed. I.e. for
// 3: 3, 2 + 1, 1 + 2, 1 + 1 + 1
let generateSummingSequences n maxLength =
    let cache = Dictionary<(int * int), (int list) list>()

    let rec go n remaining =
        let key = (n, remaining)

        if cache.ContainsKey(key) then
            cache.[key]
        else
            let result =
                match n with
                | 0 -> [ [] ]
                | n ->
                    [ for x = 1 to min remaining n do
                          for seq in go (n - x) (remaining - x) do
                              if Seq.length seq < maxLength then
                                  yield x :: seq ]

            cache.Add(key, result)
            result

    go n n

// https://stackoverflow.com/a/3129136
let rec distribute e =
    function
    | [] -> [ [ e ] ]
    | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

let rec permute =
    function
    | [] -> [ [] ]
    | e :: xs -> List.collect (distribute e) (permute xs)

let parseLine line =
    let pNegative = pchar '-' >>. pint32 |>> (fun x -> -x)
    let pNumber = choice [ pNegative; pint32 ]
    let pNotNumber = regex "[^0-9\-]+"
    let pEntry = pNotNumber >>. pNumber

    let pLine =
        many pEntry
        |>> (fun numbers ->
            let numArray = List.toArray numbers

            { capacity = numArray[0]
              durability = numArray[1]
              flavor = numArray[2]
              texture = numArray[3]
              calories = numArray[4] })

    match run pLine line with
    | Success(r, _, _) -> r
    | _ -> failwithf "Failed to parse line '%s'" line

let mult ingredient n =
    { capacity = ingredient.capacity * n
      durability = ingredient.durability * n
      flavor = ingredient.flavor * n
      texture = ingredient.texture * n
      calories = ingredient.calories * n }

let add i1 i2 =
    { capacity = i1.capacity + i2.capacity
      durability = i1.durability + i2.durability
      flavor = i1.flavor + i2.flavor
      texture = i1.texture + i2.texture
      calories = i1.calories + i2.calories }

let getScore permutation summingSequence =
    let permArray = List.toArray permutation

    let total =
        summingSequence |> List.mapi (fun i n -> mult permArray[i] n) |> List.reduce add

    let scores = [| total.capacity; total.durability; total.flavor; total.texture |]

    if Array.exists (fun x -> x < 0) scores then
        total.calories, 0
    else
        total.calories, Array.reduce (fun a b -> a * b) scores

(*
    The idea is to generate all ways we can sum X ingredients to 100, and
    generate all permutations of ingredients. We then take the product of the
    two, and calculate scores.
*)
let run filename =
    let input =
        filename |> System.IO.File.ReadAllLines |> Seq.map parseLine |> Seq.toList

    let permutations = permute input
    let summingSequences = generateSummingSequences 100 (List.length input)

    let scores =
        permutations
        |> List.collect (fun x -> List.map (fun y -> (x, y)) summingSequences)
        |> List.map (fun (x, y) -> getScore x y)

    let part1 = scores |> List.maxBy snd |> snd

    let part2 = scores |> List.filter ((=) 500 << fst) |> List.maxBy snd |> snd

    printfn "Part 1: %i" part1
    printfn "Part 2: %i" part2
