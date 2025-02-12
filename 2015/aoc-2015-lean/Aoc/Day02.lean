def Box := Int × Int × Int


def parseLine! (line : String) :=
  match line.splitOn "x" with
  | [l, w, h] => (l.toInt!, w.toInt!, h.toInt!)
  | _ => panic "Invalid input"


def area (box : Box) : Int :=
  let (l, w, h) := box
  let sides := [l * w, w * h, h * l]
  let smallestSide := sides.min?.get!

  smallestSide + (sides.map (· * 2) |>.sum)


def ribbon (box : Box) : Int :=
  let (l, w, h) := box
  let volume := l * w * h
  let perimeter := [l, w, h] |>.mergeSort |>.take 2 |>.sum

  volume + perimeter * 2


def main : IO Unit := do
  let input ← IO.FS.readFile "input/02.txt"
  let input := input.trim.splitOn "\n" |>.map parseLine!

  let part1 := input |>.map area |>.sum
  let part2 := input |>.map ribbon |>.sum

  IO.println s!"Part 1: {part1}"
  IO.println s!"Part 2: {part2}"
