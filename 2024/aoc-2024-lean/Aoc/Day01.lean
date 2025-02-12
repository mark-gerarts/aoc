def parseInput (input: String): List Int × List Int :=
    let pairs := input.trim.splitOn "\n" |>.map (·.splitOn "   ")
    let lefts := pairs |>.map (·[0]!.toInt!)
    let rights := pairs |>.map (·[1]!.toInt!)

    (lefts, rights)


def part1 (lefts rights: List Int): Nat :=
    List.zip lefts.mergeSort rights.mergeSort
    |>.map (λ (l, r) => l - r)
    |>.map Int.natAbs
    |>.sum


def part2 (lefts rights: List Int): Int :=
  let similarityScore l := rights |>.filter (· = l) |>.length

  lefts
  |>.map (λ l => l * similarityScore l)
  |>.sum


def main : IO Unit := do
  let input ← IO.FS.readFile "input/01.txt"
  let (ls, rs) := parseInput input

  IO.println s!"Part 1: {part1 ls rs}"
  IO.println s!"Part 2: {part2 ls rs}"
