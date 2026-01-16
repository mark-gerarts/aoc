open System.Text.RegularExpressions


let unzip line =
    let rec go (remainder: string) =
        seq {
            let nextMarker = Regex("\((\d+)x(\d+)\)").Match remainder

            match nextMarker.Groups |> Seq.map _.Value |> Seq.toArray with
            | [| _; charCount; repeat |] ->
                yield remainder[0 .. nextMarker.Index - 1]

                let repeatSectionStart = nextMarker.Index + nextMarker.Length
                let repeatSectionEnd = repeatSectionStart + int charCount
                let sectionToRepeat = remainder[repeatSectionStart .. repeatSectionEnd - 1]
                yield String.replicate (int repeat) sectionToRepeat

                yield! go remainder[repeatSectionEnd..]
            | _ -> yield remainder
        }

    go line |> System.String.Concat

unzip "ADVENT" |> printfn "%A"
unzip "A(1x5)BC" |> printfn "%A"
unzip "(3x3)XYZ" |> printfn "%A"
unzip "A(2x2)BCD(2x2)EFG" |> printfn "%A"
unzip "(6x1)(1x3)A" |> printfn "%A"
unzip "X(8x2)(3x3)ABCY" |> printfn "%A"

System.IO.File.ReadAllText "input/09.txt"
|> _.Trim()
|> unzip
|> _.Length
|> printfn "Part 1: %i"
