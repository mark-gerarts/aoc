def toLevels (input: String): List Int :=
  let parse
    | '(' => 1
    | ')' => -1
    | _ => panic "Invalid input"

  input |>.toList |>.map parse


def scanl (f: β → α → β) (x: β)
  | [] => [x]
  | y :: ys => x :: scanl f (f x y) ys


def part1 (input: String): Int :=
  input |> toLevels |>.sum


def part2 (input: String): Int :=
  input
  |> toLevels
  |> scanl (· + ·)  0
  |>.takeWhile (· >= 0)
  |>.length


def main : IO Unit := do
  let input ← IO.FS.readFile "input/01.txt"
  IO.println s!"Part 1: {part1 input}"
  IO.println s!"Part 2: {part2 input}"
