#r "nuget: FsUnit,6.0.0"

open FsUnit

type HandType =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let handType hand =
    let cardCounts =
        hand
        |> Seq.toList
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.sortByDescending id
        |> Seq.toList

    match cardCounts with
    | [ 5 ] -> FiveOfAKind
    | [ 4; 1 ] -> FourOfAKind
    | [ 3; 2 ] -> FullHouse
    | [ 3; 1; 1 ] -> ThreeOfAKind
    | [ 2; 2; 1 ] -> TwoPair
    | [ 2; 1; 1; 1 ] -> OnePair
    | _ -> HighCard

let rank card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> int c - int '0'

let rec compareCards rankFn (hand1: string) (hand2: string) =
    let hand1 = Seq.map rankFn hand1
    let hand2 = Seq.map rankFn hand2

    Seq.compareWith compare hand1 hand2

let compareHands handTypeFn rankFn (hand1: string) (hand2: string) =
    let comparison =
        match compare (handTypeFn hand1) (handTypeFn hand2) with
        | 0 -> compareCards rankFn hand1 hand2
        | comparison -> comparison

    match comparison with
    | c when c > 0 -> 1
    | c when c < 0 -> -1
    | _ -> 0

let parseLine (line: string) =
    match line.Split(' ') with
    | [| hand; bid |] -> hand, int bid
    | _ -> failwithf "Failed to parse line %s" line

let solve handTypeFn rankFn =
    "input/07.test"
    |> System.IO.File.ReadLines
    |> Seq.map parseLine
    |> Seq.sortWith (fun a b -> compareHands handTypeFn rankFn (fst a) (fst b))
    |> Seq.mapi (fun i (_, bid) -> (i + 1) * bid)
    |> Seq.sum

//
// Part 2
//

let jokerize hand =
    let replacements = [ '2' .. '9' ] @ [ 'T'; 'Q'; 'K'; 'A' ]

    let rec variations hand =
        seq {
            match hand with
            | [] -> yield []
            | 'J' :: rest ->
                for r in replacements do
                    for rest' in variations rest do
                        yield r :: rest'
            | c :: rest ->
                for rest' in variations rest do
                    yield c :: rest'
        }

    variations (Seq.toList hand)

let handTypeP2 hand =
    hand |> jokerize |> Seq.map handType |> Seq.max

let rankP2 card =
    match card with
    | 'J' -> 1
    | c -> rank c

solve handType rank |> printfn "Part 1: %i"
solve handTypeP2 rankP2 |> printfn "Part 2: %i"

handType "QQQQQ" |> should equal FiveOfAKind
handType "QQQQ1" |> should equal FourOfAKind
handType "QQQKK" |> should equal FullHouse
handType "QQKKK" |> should equal FullHouse
handType "Q1Q2Q" |> should equal ThreeOfAKind
handType "77122" |> should equal TwoPair
handType "912K9" |> should equal OnePair
handType "12345" |> should equal HighCard

compareCards rank "A" "K" |> should equal 1
compareCards rank "3" "9" |> should equal -1
compareCards rank "T" "T" |> should equal 0

compareHands handType rank "KK677" "KTJJT" |> should equal 1
compareHands handType rank "T55J5" "QQQJA" |> should equal -1

compareCards rankP2 "J" "2" |> should equal -1
