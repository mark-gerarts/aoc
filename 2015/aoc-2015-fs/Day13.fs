module AoC2015.Day13

open FParsec

type Name = string

type InputLine =
    { subject: Name
      amount: int
      neighbour: Name }

type HappinessMap = Map<(Name * Name), int>

let parseLine line =
    let pName = regex "[A-Z][a-z]+"
    let pPlus = pstring "gain " >>. pint32
    let pMinus = pstring "lose " >>. pint32 |>> (fun gain -> -gain)
    let pGain = choice [ pPlus; pMinus ]

    let pLine =
        pipe3
            pName
            (pstring " would " >>. pGain)
            (pstring " happiness units by sitting next to " >>. pName)
            (fun a b c ->
                { subject = a
                  amount = b
                  neighbour = c })

    match run pLine line with
    | Success(r, _, _) -> r
    | _ -> failwithf "Could not parse line '%s'" line

// https://stackoverflow.com/a/3129136
let rec distribute e =
    function
    | [] -> [ [ e ] ]
    | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

let rec permute =
    function
    | [] -> [ [] ]
    | e :: xs -> List.collect (distribute e) (permute xs)

let addInputLine map line =
    Map.add (line.subject, line.neighbour) line.amount map

let calculateHappiness happinessMap (ordering: Name list) =
    let happinessForPair (a, b) =
        Map.find (a, b) happinessMap + Map.find (b, a) happinessMap

    let circular = List.append ordering [ List.head ordering ]

    circular |> Seq.pairwise |> Seq.map happinessForPair |> Seq.sum

let getPeople happinessMap =
    happinessMap |> Map.keys |> Seq.map fst |> Seq.distinct |> Seq.toList

let solve happinessMap =
    happinessMap
    |> getPeople
    |> permute
    |> Seq.map (calculateHappiness happinessMap)
    |> Seq.max

let insertMe happinessMap =
    let me = "Me"

    let insertForOtherPerson map other =
        map |> Map.add (other, me) 0 |> Map.add (me, other) 0

    happinessMap |> getPeople |> List.fold insertForOtherPerson happinessMap


let run filename =
    let happinessMap =
        filename
        |> System.IO.File.ReadLines
        |> Seq.map parseLine
        |> Seq.fold addInputLine Map.empty

    happinessMap |> solve |> printfn "Part 1: %i"
    happinessMap |> insertMe |> solve |> printfn "Part 2: %i"
