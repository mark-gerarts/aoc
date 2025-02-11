def parseInput (input: String): List Int × List Int :=
    let pairs :=
      input.trim.splitOn "\n"
      |> List.map (·.splitOn "   ")

    let lefts := pairs |> List.map (·[0]!.toInt!)
    let rights := pairs |> List.map (·[1]!.toInt!)

    (lefts, rights)


def part1 (lefts rights: List Int): Nat :=
    List.zip (List.mergeSort lefts) (List.mergeSort rights)
    |> List.map (λ (l, r) => l - r)
    |> List.map Int.natAbs
    |> List.sum


def part2 (lefts rights: List Int): Int :=
  let similarityScore l :=
    rights |> List.filter (· = l) |> List.length

  lefts
  |> List.map (λ l => l * similarityScore l)
  |> List.sum


def main : IO Unit := do
  let input ← IO.FS.readFile "input/01.txt"
  let (ls, rs) := parseInput input

  s!"Part 1: {part1 ls rs}" |> IO.println
  s!"Part 2: {part2 ls rs}" |> IO.println
