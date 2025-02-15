import Regex


def mulRegex := regex% r"mul\((\d+),(\d+)\)"


inductive Operation where
  | do : Operation
  | dont : Operation
  | mul : Int -> Int -> Operation
deriving Repr


def parseDigitMatch (m : Option Regex.Match) :=
  (·.toString) <$> m >>= (·.toInt?)


def Operation.parse (input : String) : Operation :=
  match input with
  | "do()" => Operation.do
  | "dont()" => Operation.dont
  | s =>
    match Regex.captures s mulRegex with
    | Option.some m =>
      match m.groups |>.map parseDigitMatch with
      | #[Option.some a, Option.some b] => Operation.mul a b
      | _ => panic "Invalid input"
    | _ => panic "Invalid input"


#eval Operation.parse "mul(1,2)"


def testInput := "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"





def part1 (input : String) :=
  let re := regex% r"mul\((\d+),(\d+)\)"
  Regex.all_captures input re
  |>.map (·.groups |>.map parseDigitMatch |>.map Option.get!)
  |>.map (λ m => m[0]! * m[1]!)
  |>.toList
  |>.sum





def main : IO Unit := do
  let input ← IO.FS.readFile "input/03.txt"
  IO.println s!"{part1 input}"

#eval main
