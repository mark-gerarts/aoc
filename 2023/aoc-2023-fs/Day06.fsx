open System.Text.RegularExpressions

let parseInput (input: string) =
    let digits s = Regex.Matches(s, "\d+") |> Seq.map (_.Value >> int64)

    match input.Split('\n') |> Array.map digits with
    | [| times; distances |] -> Seq.zip times distances
    | _ -> failwithf "Could not parse input"

let getPossibleActions duration =
    seq {
        for holdDownMs in [1L..duration - 1L] do
            let durationLeft = duration - holdDownMs
            let distanceTravelled = holdDownMs * durationLeft

            yield distanceTravelled
    }

let getNumberOfWinningActions (duration, distance) =
    getPossibleActions duration
    |> Seq.filter ((<) distance)
    |> Seq.length

let solve input =
    input |> parseInput |> Seq.map getNumberOfWinningActions |> Seq.reduce (*)

let input = System.IO.File.ReadAllText "input/06.txt" |> _.Trim()
input |> solve |> printfn "Part 1: %i"
input |> _.Replace(" ", "") |> solve |> printfn "Part 2: %i"
