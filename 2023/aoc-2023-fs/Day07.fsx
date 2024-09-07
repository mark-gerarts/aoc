#r "nuget: FsUnit,6.0.0"

open FsUnit

type HandType =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let handType hand =
    let cards = Seq.toList hand
    let cardCounts = cards |> Seq.countBy id |> Seq.map snd |> Seq.toList

    let numPairs cardCounts =
        cardCounts
        |> List.countBy id
        |> Map.ofList
        |> Map.tryFind 2
        |> Option.defaultValue 0

    let matchers =
        [ cardCounts = [ 5 ], FiveOfAKind
          List.contains 4 cardCounts, FourOfAKind
          cardCounts = [ 3; 2 ], FullHouse
          cardCounts = [ 2; 3 ], FullHouse
          List.contains 3 cardCounts, ThreeOfAKind
          numPairs cardCounts = 2, TwoPair
          numPairs cardCounts = 1, OnePair
          true, HighCard ]

    matchers |> List.find ((fst)) |> snd

let rank card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> int c - int '0'

let rec compareCards (hand1: string) (hand2: string) =
    let hand1 = Seq.map rank hand1
    let hand2 = Seq.map rank hand2

    Seq.compareWith compare hand1 hand2

let compareHands (hand1: string) (hand2: string) =
    let comparison =
        match compare (handType hand1) (handType hand2) with
        | 0 -> compareCards hand1 hand2
        | comparison -> comparison

    match comparison with
    | c when c > 0 -> 1
    | c when c < 0 -> -1
    | _ -> 0

let parseLine (line: string) =
    match line.Split(' ') with
    | [| hand; bid |] -> hand, int bid
    | _ -> failwithf "Failed to parse line %s" line

let input = System.IO.File.ReadLines "input/07.test" |> Seq.map parseLine

input
|> Seq.sortWith (fun a b -> compareHands (fst a) (fst b))
|> Seq.rev
|> Seq.toList
|> printfn "%A"

handType "QQQQQ" |> should equal FiveOfAKind
handType "QQQQ1" |> should equal FourOfAKind
handType "QQQKK" |> should equal FullHouse
handType "QQKKK" |> should equal FullHouse
handType "Q1Q2Q" |> should equal ThreeOfAKind
handType "77122" |> should equal TwoPair
handType "912K9" |> should equal OnePair
handType "12345" |> should equal HighCard

compareCards "A" "K" |> should equal 1
compareCards "3" "9" |> should equal -1
compareCards "T" "T" |> should equal 0

compareHands "KK677" "KTJJT" |> should equal 1
compareHands "T55J5" "QQQJA" |> should equal -1
