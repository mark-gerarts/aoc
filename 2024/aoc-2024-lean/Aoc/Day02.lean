abbrev Report := List Int


def parseInput (input : String) : List Report :=
  input.trim.splitOn "\n"
  |>.map (·.splitOn " " |>.map String.toInt!)


def isSafe (report : Report) : Bool :=
  let allIncreasing := report.mergeSort = report
  let allDecreasing := report.mergeSort.reverse = report
  let diffCheck :=
    List.zip report report.tail
    |>.map (λ (a, b) => (a - b).natAbs)
    |>.all (λ diff => diff ≥ 1 ∧ diff ≤ 3)

  (allIncreasing ∨ allDecreasing) ∧ diffCheck


def part1 (reports : List Report) : Int :=
  reports |>.filter isSafe |>.length


def deletePermutations (xs : List α) : List (List α) :=
  xs.enum.map Prod.fst |>.map (λ i => xs.take i ++ xs.drop (i + 1))


def part2 (reports : List Report) :=
  reports |>.filter (deletePermutations · |>.any isSafe) |>.length


def main : IO Unit := do
  let input := (← IO.FS.readFile "input/02.txt") |> parseInput
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"
