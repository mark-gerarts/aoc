import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

type Direction {
  Left
  Right
}

type Instruction =
  #(Direction, Int)

pub fn main() -> Nil {
  let assert Ok(raw_input) = simplifile.read("input/01.txt")
  let instructions = parse_input(raw_input)

  let dial_states = run_instructions(instructions)
  let solution_part_1 = part_1(dial_states) |> int.to_string
  let solution_part_2 = part_2(dial_states) |> int.to_string

  io.println("Part 1: " <> solution_part_1)
  io.println("Part 2: " <> solution_part_2)
}

// The idea is that we list.scan to get every intermediate dial state, and then
// count the last dial state per instruction for part 1 and everything for part
// 2.
fn part_1(dial_states) {
  dial_states
  |> list.map(fn(ds) { list.last(ds) |> result.unwrap(0) })
  |> list.count(fn(x) { x == 0 })
}

fn part_2(dial_states) {
  dial_states
  |> list.flat_map(function.identity)
  |> list.count(fn(x) { x == 0 })
}

fn rotate_one(dial, direction) {
  case dial, direction {
    99, Right -> 0
    _, Right -> dial + 1
    0, Left -> 99
    _, Left -> dial - 1
  }
}

fn run_instruction(dial: Int, instruction: Instruction) -> List(Int) {
  let #(direction, amount) = instruction
  list.repeat(direction, amount)
  |> list.scan(from: dial, with: rotate_one)
}

fn run_instructions(instructions: List(Instruction)) {
  instructions
  |> list.scan(from: [50], with: fn(acc, instruction) {
    let assert Ok(dial) = list.last(acc)
    run_instruction(dial, instruction)
  })
}

fn parse_line(line: String) -> Instruction {
  case line {
    "L" <> amount -> {
      let assert Ok(amount) = int.parse(amount)
      #(Left, amount)
    }
    "R" <> amount -> {
      let assert Ok(amount) = int.parse(amount)
      #(Right, amount)
    }
    _ -> panic as "Unexpected input line"
  }
}

fn parse_input(raw_input: String) -> List(Instruction) {
  raw_input |> string.trim() |> string.split("\n") |> list.map(parse_line)
}
