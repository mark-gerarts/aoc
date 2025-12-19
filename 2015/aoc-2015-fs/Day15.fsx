open System.Text.RegularExpressions

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
    let rec solve currentSum currentLen acc =
        [ if currentSum = n then
              yield List.rev acc
          elif currentLen < maxLength then
              for x = 1 to n - currentSum do
                  yield! solve (currentSum + x) (currentLen + 1) (x :: acc) ]

    solve 0 0 []

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
    let numbers = Regex("-?\d").Matches line |> Seq.map (_.Value >> int) |> Seq.toArray

    { capacity = numbers[0]
      durability = numbers[1]
      flavor = numbers[2]
      texture = numbers[3]
      calories = numbers[4] }

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
    let total =
        summingSequence
        |> List.mapi (fun i n -> mult (List.item i permutation) n)
        |> List.reduce add

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
let input =
    System.IO.File.ReadAllLines "input/15.txt" |> Seq.map parseLine |> Seq.toList

let permutations = permute input

let summingSequences = generateSummingSequences 100 (List.length input)

let scores =
    permutations
    |> List.collect (fun x -> List.map (fun y -> (x, y)) summingSequences)
    |> List.map (fun (x, y) -> getScore x y)

let part1 = scores |> List.maxBy snd |> snd
let part2 = scores |> List.filter (fst >> (=) 500) |> List.maxBy snd |> snd

printfn "Part 1: %i" part1
printfn "Part 2: %i" part2
