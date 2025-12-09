open System.Text.RegularExpressions

let parseOp c =
    match c with
    | '*' -> (*), 1L
    | '+' -> (+), 0L
    | _ -> failwithf "Unexpected symbol %c when parsing operation" c


let inputLines = System.IO.File.ReadLines "input/06.txt"
let numberLines = inputLines |> Seq.rev |> Seq.tail |> Seq.rev
let operations = inputLines |> Seq.last |> _.Replace(" ", "") |> Seq.map parseOp

let parsePart1 =
    numberLines
    |> Seq.map (_.Trim() >> (fun s -> Regex.Split(s, " +")) >> Seq.map int64)
    |> Seq.transpose

let solve parsedNumbers =
    Seq.zip operations parsedNumbers
    |> Seq.sumBy (fun ((op, id), numbers) -> Seq.fold op id numbers)

solve parsePart1 |> printfn "Part 1: %i"

let splitOn pred source =
    let folder acc item =
        if pred item then
            [] :: acc
        else
            match acc with
            | [] -> [ [ item ] ]
            | head :: tail -> (item :: head) :: tail

    source |> Seq.fold folder [] |> List.map List.rev |> List.rev

let tryParseInt (s: string) =
    match System.Int64.TryParse s with
    | true, value -> Some value
    | _ -> None

let parsePart2 =
    let charSeqToNumber seq =
        Seq.filter ((<>) ' ') seq |> System.String.Concat |> tryParseInt

    numberLines
    |> Seq.map (Seq.map id)
    |> Seq.transpose
    |> Seq.map charSeqToNumber
    |> splitOn Option.isNone
    |> Seq.map (Seq.choose id)

solve parsePart2 |> printfn "Part 2: %i"
