@enum Direction Left Right

struct Instruction
    direction::Direction
    amount::Int
end

function parse_line(line::String)
    @assert length(line) >= 2
    @assert line[1] == 'L' || line[1] == 'R'

    direction = line[1] == 'L' ? Left : Right
    amount = parse(Int, line[2:end])

    Instruction(direction, amount)
end

function solve(filename::String)
    instructions = readlines(filename) .|> parse_line
    dial = 50
    num_zeroes_part1 = 0
    num_zeroes_part2 = 0

    for instruction in instructions
        direction = instruction.direction
        amount = instruction.amount
        step = direction == Left ? -1 : 1

        for _ in 1:amount
            dial = mod(dial + step, 100)
            num_zeroes_part2 += dial == 0
        end

        num_zeroes_part1 += dial == 0
    end

    println("Part 1: $num_zeroes_part1")
    println("Part 2: $num_zeroes_part2")
end

solve("input/01.txt")
