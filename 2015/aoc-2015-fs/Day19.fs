module AoC2015.Day19

let parseInput filename =
    let input = System.IO.File.ReadAllText filename
    let parts = input.Trim().Split("\n\n")
    let replacements = parts[0]
    let inputString = parts[1]

    let parseReplacementLine (line: string) =
        let parts = line.Split(" => ")
        (parts[0], parts[1])

    (replacements.Split('\n') |> Seq.map parseReplacementLine |> Seq.toList, inputString)

let generateMolecules (replacements: (string * string) list) (input: string) =
    let rec go (replacements: (string * string) list) (currentIndex: int) =
        match replacements with
        | [] -> []
        | (l, r) :: xs ->
            match input.IndexOf(l, currentIndex) with
            | -1 -> go xs 0
            | index ->
                let newMolecule = input.Remove(index, String.length l).Insert(index, r)
                newMolecule :: go replacements (index + 1)

    go replacements 0 |> List.distinct

let rec findShortestPath replacements inputs target numSteps =
    let allNewMolecules =
        inputs
        |> List.collect (generateMolecules replacements)
        // Replacements only make the string larger, so we can cull things.
        |> List.filter (fun m -> String.length m < String.length target)

    printfn "%A" (List.length allNewMolecules)


    if List.contains target allNewMolecules then
        numSteps + 1
    else
        findShortestPath replacements allNewMolecules target (numSteps + 1)

let run filename =
    let (replacements, input) = parseInput filename
    generateMolecules replacements input |> List.length |> printfn "Part 1: %i"

    findShortestPath replacements [ "e" ] input 0 |> printfn "Part 2: %i"
