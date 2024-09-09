type Direction =
    | Left
    | Right

let parseInput (input: string) =
    let parseLrs (line: string) =
        line
        |> Seq.map (fun c ->
            match c with
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwithf "Invalid step")
        |> Seq.toList

    let parseNodeLine (line: string) =
        line.Substring(0, 3), (line.Substring(7, 3), line.Substring(12, 3))

    let parseNodeList (nodeList: string) =
        nodeList.Split('\n') |> Seq.map parseNodeLine |> Map.ofSeq

    match input.Trim().Split("\n\n") with
    | [| lrs; nodes |] -> parseLrs lrs, parseNodeList nodes
    | _ -> failwithf "Invalid input"

let followStep nodeList currentNode step =
    let (leftNode, rightNode) = Map.find currentNode nodeList

    match step with
    | Left -> leftNode
    | Right -> rightNode

let (steps, nodeList) = System.IO.File.ReadAllText "input/08.txt" |> parseInput

let pathLength endCond start =
    let infiniteSteps =
        Seq.initInfinite (fun i -> List.item (i % List.length steps) steps)

    infiniteSteps
    |> Seq.scan (followStep nodeList) start
    |> Seq.takeWhile (not << endCond)
    |> Seq.length

// https://gist.github.com/krishnabhargav/da6686e295638d000aab
let rec gcd a b =
    match (a,b) with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y) -> gcd x (y-x)

// You have to observe the data to know that LCM is a valid solution...
let lcm a b =
    a*b / (gcd a b)

pathLength (_.Equals("ZZZ")) "AAA" |> printfn "Part 1: %i"

nodeList
|> Map.keys
|> Seq.filter (fun s -> s[2] = 'A')
|> Seq.map (pathLength (fun s -> s[2] = 'Z'))
|> Seq.map int64
|> Seq.reduce lcm
|> printfn "Part 2: %i"
