module AoC2015.Day03

type Pos = int * int

let inline (+?) (a: Pos) (b: Pos) = (fst a + fst b, snd a + snd b)

type State = { seen: Set<Pos>; current: Pos }

let initialState =
    { seen = Set.singleton (0, 0)
      current = (0, 0) }

let parseMove c =
    match c with
    | '^' -> (0, +1)
    | 'v' -> (0, -1)
    | '>' -> (+1, 0)
    | '<' -> (-1, 0)
    | _ -> failwithf "Invalid character %c" c

let applyMove (state: State) (move: Pos) =
    let newPos = state.current +? move

    { seen = state.seen.Add newPos
      current = newPos }

let parseFile file =
    file |> System.IO.File.ReadLines |> Seq.head |> Seq.map parseMove

let applyInstructions instructions =
    instructions |> Seq.fold applyMove initialState

let part1 file =
    file |> parseFile |> applyInstructions |> (fun s -> s.seen) |> Set.count

let part2 file =
    file
    |> parseFile
    |> Seq.indexed
    |> Seq.map (fun (i, x) -> (i % 2, x))
    |> Seq.groupBy fst
    |> Seq.map (snd >> Seq.map snd)
    |> Seq.map applyInstructions
    |> Seq.map (fun s -> s.seen)
    |> Set.unionMany
    |> Set.count

let run file =
    printfn "Part 1: %i" <| part1 file
    printfn "Part 2: %i" <| part2 file
