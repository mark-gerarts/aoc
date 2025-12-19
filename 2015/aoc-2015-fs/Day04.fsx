let mine numZeros secretKey =
    use md5 = System.Security.Cryptography.MD5.Create()

    let hash (input: string) =
        input
        |> System.Text.Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> Seq.map _.ToString("X2")
        |> System.String.Concat

    let expectedPrefix = String.replicate numZeros "0"

    let rec go input i =
        let hash = hash (input + string i)
        let prefix = hash[.. numZeros - 1]

        if prefix.Equals expectedPrefix then i else go input (i + 1)

    go secretKey 0

// Runs in ~3s singlethreaded.
let input = System.IO.File.ReadAllText "input/04.txt" |> _.Trim()
input |> mine 5 |> printfn "Part 1: %i"
input |> mine 6 |> printfn "Part 2: %i"
