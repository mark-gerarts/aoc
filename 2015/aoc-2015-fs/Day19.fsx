type Replacements = (string * string) list

let parseInput =
    let parts = System.IO.File.ReadAllText "input/19.txt" |> _.Trim() |> _.Split("\n\n")
    let replacements = parts[0]
    let inputString = parts[1]

    let parseReplacementLine (line: string) =
        let parts = line.Split " => "
        parts[0], parts[1]

    replacements.Split '\n' |> Seq.map parseReplacementLine |> Seq.toList, inputString

let generateMolecules (replacements: Replacements) (input: string) =
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

let reverseMolecule (replacements: Replacements) (molecule: string) =
    let rec go (molecule: string) (currentReplacements: Replacements) (steps: int) =
        match currentReplacements with
        | [] -> steps
        | (l, r) :: xs ->
            match molecule.IndexOf r with
            | -1 -> go molecule xs steps
            | index ->
                let newMolecule = molecule.Remove(index, String.length r).Insert(index, l)
                go newMolecule replacements (steps + 1)

    go molecule replacements 0

let replacements, input = parseInput

generateMolecules replacements input |> List.length |> printfn "Part 1: %i"
reverseMolecule replacements input |> printfn "Part 2: %i"
