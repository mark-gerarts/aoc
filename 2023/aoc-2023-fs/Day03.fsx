open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText "input/03.txt"

let width =
    let lines = input.Trim().Split('\n')
    // +1 to account for the newlines that are still included in the regex
    // parsing.
    lines[0].Length + 1

let indexToCoord index = (index % width, index / width)

let numbers =
    // Adds the number at each position it occupies. We add the matching index
    // to be able to filter on unique numbers later on.
    let collectNumber (m: Match) =
        let (x, y) = indexToCoord m.Index
        let length = m.Value.Length
        let number = int m.Value

        List.init length (fun i -> (x + i, y), (m.Index, number))

    Regex.Matches(input, "\d+")
    |> Seq.collect collectNumber
    |> Map.ofSeq

let partPositions =
    Regex.Matches(input, "[^\.\d\\n]")
    |> Seq.map (_.Index >> indexToCoord)

let gearPositions =
    Regex.Matches(input, "\*")
    |> Seq.map (_.Index >> indexToCoord)

let neighbours (x, y) =
    [(x - 1, y - 1);
    (x - 1, y);
    (x - 1, y + 1);
    (x, y - 1);
    (x, y + 1);
    (x + 1, y - 1);
    (x + 1, y);
    (x + 1, y + 1)]

partPositions
|> Seq.collect neighbours
|> Seq.choose (fun p -> Map.tryFind p numbers)
|> Seq.distinct
|> Seq.sumBy snd
|> printfn "Part 1: %i"

gearPositions
|> Seq.map (neighbours >> Seq.choose (fun p -> Map.tryFind p numbers) >> Seq.distinct)
|> Seq.filter (Seq.length >> ((=) 2))
|> Seq.sumBy (Seq.map snd >> Seq.reduce (*))
|> printfn "Part 2: %i"
