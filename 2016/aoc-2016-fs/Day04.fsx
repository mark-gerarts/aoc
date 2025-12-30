let parseLine (line: string) =
    match line.Split '[' with
    | [| first; second |] ->
        let parts = first.Split '-'
        let code = Array.last parts
        let name = parts |> Array.rev |> Array.tail |> Array.rev |> System.String.Concat
        let checksum = second |> Seq.take 5 |> System.String.Concat

        name, int code, checksum
    | _ -> failwithf "Invalid input line %s" line

let isValidLine (name, _, checksum) =
    let calculatedChecksum =
        name
        |> Seq.groupBy id
        |> Seq.map (fun (char, list) -> char, Seq.length list)
        |> Seq.filter (fst >> (<>) '-')
        |> Seq.sortBy (fun (char, count) -> -count, char)
        |> Seq.take 5
        |> Seq.map fst
        |> System.String.Concat

    calculatedChecksum.Equals checksum

let input = System.IO.File.ReadAllLines "input/04.txt" |> Seq.map parseLine

input
|> Seq.filter isValidLine
|> Seq.sumBy (fun (_, id, _) -> id)
|> printfn "Part 1: %i"

let rotateChar amount chr =
    if chr = '-' then
        ' '
    else
        let charNum = int chr - int 'a'
        let newCharNum = (charNum + amount) % 26

        char (newCharNum + int 'a')

let decryptLine name id =
    name |> Seq.map (rotateChar id) |> System.String.Concat

input
|> Seq.map (fun (name, id, _) -> id, decryptLine name id)
|> Seq.find (snd >> _.Equals("northpoleobjectstorage"))
|> fst
|> printfn "Part 1: %i"
