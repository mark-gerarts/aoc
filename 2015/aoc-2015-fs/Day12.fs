module AoC2015.Day12

open System.Text.Json

let isRedValue (json: JsonProperty) =
    let value = json.Value

    match value.ValueKind with
    | JsonValueKind.String -> value.GetString().Equals("red")
    | _ -> false

let containsRedValue (json: JsonElement) =
    json.EnumerateObject() |> Seq.exists isRedValue

let rec collectNumbers (filter: JsonElement -> bool) (json: JsonElement) =
    seq {
        match json.ValueKind with
        | JsonValueKind.Number -> yield json.GetInt32()
        | JsonValueKind.Object when filter json ->
            yield!
                json.EnumerateObject()
                |> Seq.map (fun el -> el.Value)
                |> Seq.collect (collectNumbers filter)
        | JsonValueKind.Array -> yield! json.EnumerateArray() |> Seq.collect (collectNumbers filter)
        | _ -> ()
    }

let run filename =
    let filterPart1 = fun _ -> true
    let filterPart2 = not << containsRedValue

    let input =
        filename
        |> System.IO.File.ReadAllText
        |> JsonSerializer.Deserialize<JsonElement>

    input |> collectNumbers filterPart1 |> Seq.sum |> printfn "Part 1: %i"
    input |> collectNumbers filterPart2 |> Seq.sum |> printfn "Part 2: %i"
