open System.Security.Cryptography
open System.Text

let hash (input: string) =
    use md5 = MD5.Create()
    let inputBytes = Encoding.UTF8.GetBytes input
    let hashBytes = md5.ComputeHash inputBytes

    System.BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

let input = System.IO.File.ReadAllText "input/05.txt" |> _.Trim()

Seq.initInfinite (sprintf "%s%i" input)
|> Seq.map hash
|> Seq.filter _.StartsWith("00000")
|> Seq.map (Seq.item 5)
|> Seq.take 8
|> System.String.Concat
|> printfn "Part 1: %s"

let hashes =
    let rec go hashes seenPositions i =
        if Set.count seenPositions = 8 then
            hashes
        else
            let hash = sprintf "%s%i" input i |> hash

            if hash.StartsWith "00000" then
                let p = hash[5]

                if p >= '0' && p <= '7' && not (Set.contains p seenPositions) then
                    go (hash :: hashes) (Set.add p seenPositions) (i + 1)
                else
                    go hashes seenPositions (i + 1)
            else
                go hashes seenPositions (i + 1)

    go [] Set.empty 0

hashes
|> List.sort
|> List.map (Seq.item 6)
|> System.String.Concat
|> printfn "Part 2: %s"
