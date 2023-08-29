module AoC2015.Day04


let md5 (input: string) =
    use md5 = System.Security.Cryptography.MD5.Create()

    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let mine numZeros input =
    let expectedPrefix = String.replicate numZeros "0"

    let rec go input i =
        let hash = md5 (input + string i)
        let prefix = hash[.. numZeros - 1]

        if prefix.Equals(expectedPrefix) then
            i
        else
            go input (i + 1)

    go input 0

let run file =
    let input = System.IO.File.ReadAllText(file).Trim()

    input |> mine 5 |> printfn "Part 1: %i"
    input |> mine 6 |> printfn "Part 2: %i"
