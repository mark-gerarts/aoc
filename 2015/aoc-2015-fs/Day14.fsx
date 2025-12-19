open System.Text.RegularExpressions

type Reindeer =
    { name: string
      v: int
      flies: int
      rests: int }

let parseLine line =
    let matches = Regex("\d+").Matches line |> Seq.toArray |> Array.map (_.Value >> int)

    let name = line.Split(" ")[0]

    { name = name
      v = matches[0]
      flies = matches[1]
      rests = matches[2] }

let runNSeconds n reindeer =
    let cycleLength = reindeer.flies + reindeer.rests
    let wholeCycles = n / cycleLength
    let remainingSeconds = n % cycleLength
    let remainingFlySeconds = min remainingSeconds reindeer.flies
    let totalFlySeconds = wholeCycles * reindeer.flies + remainingFlySeconds

    totalFlySeconds * reindeer.v

let solvePart1 reindeer =
    reindeer |> Seq.map (runNSeconds 2503) |> Seq.max

let solvePart2 reindeer =
    // What we do is:
    // - For every second in 1..2503
    // - Run runNSeconds to get the intermediate distances
    // - Map each distance to 1 or 0 depending on if the distance is max or not
    // - Sum these lists together
    let intermediateScore s =
        let distances = reindeer |> Seq.map (runNSeconds s)
        let max = Seq.max distances

        Seq.map (fun d -> if d = max then 1 else 0) distances

    { 1..2503 }
    |> Seq.map intermediateScore
    |> Seq.reduce (fun a b -> Seq.map2 (+) a b)
    |> Seq.max

let reindeer = System.IO.File.ReadLines "input/14.txt" |> Seq.map parseLine

reindeer |> solvePart1 |> printfn "Part 1: %i"
reindeer |> solvePart2 |> printfn "Part 2: %i"
