let down (x, y) = x, y + 1
let left (x, y) = x - 1, y
let right (x, y) = x + 1, y

let input = System.IO.File.ReadLines "input/07.txt"

let startPos = input |> Seq.head |> _.IndexOf("S"), 0
let firstRow = (startPos, 1L) |> Seq.singleton |> Map.ofSeq

let splitterPositions =
    seq {
        for y, line in input |> Seq.indexed do
            for x, cell in Seq.indexed line do
                if cell = '^' then
                    x, y
    }
    |> Set.ofSeq

let _, maxY = splitterPositions |> Set.maxElement

// Mutable counter was just easier...
let mutable numSplits = 0

let nextRow prevRow =
    let newRow =
        seq {
            for pos, count in prevRow |> Map.toSeq do
                if Set.contains (down pos) splitterPositions then
                    numSplits <- numSplits + 1
                    yield left (down pos), count
                    yield right (down pos), count
                else
                    yield down pos, count
        }

    newRow
    |> Seq.groupBy fst
    |> Seq.map (fun (p, cs) -> p, Seq.sumBy snd cs)
    |> Map.ofSeq

let lastRow = Seq.fold (fun row _ -> nextRow row) firstRow { 0..maxY }

printfn "Part 1: %i" numSplits
lastRow |> Map.values |> Seq.sum |> printfn "Part 2: %i"
