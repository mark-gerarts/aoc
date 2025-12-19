open System.Text.Json

let containsRedValue (json: JsonElement) =
    json.EnumerateObject()
    |> Seq.exists (fun json -> json.Value.ToString().Equals "red")

let rec collectNumbers (filter: JsonElement -> bool) (json: JsonElement) =
    seq {
        match json.ValueKind with
        | JsonValueKind.Number -> yield json.GetInt32()
        | JsonValueKind.Object when filter json ->
            yield! json.EnumerateObject() |> Seq.map _.Value |> Seq.collect (collectNumbers filter)
        | JsonValueKind.Array -> yield! json.EnumerateArray() |> Seq.collect (collectNumbers filter)
        | _ -> ()
    }

let filterPart1 = fun _ -> true
let filterPart2 = not << containsRedValue

let input =
    System.IO.File.ReadAllText "input/12.txt"
    |> JsonSerializer.Deserialize<JsonElement>

input |> collectNumbers filterPart1 |> Seq.sum |> printfn "Part 1: %i"
input |> collectNumbers filterPart2 |> Seq.sum |> printfn "Part 2: %i"
