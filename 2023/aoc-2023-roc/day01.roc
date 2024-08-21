app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    unicode: "https://github.com/roc-lang/unicode/releases/download/0.1.1/-FQDoegpSfMS-a7B0noOnZQs3-A2aq9RSOR5VVLMePg.tar.br",
}

import pf.Stdout
import pf.Task
import pf.File
import unicode.Grapheme

main =
    input = File.readUtf8! "input/01.txt"
        |> Str.trim
        |> Str.split "\n"

    part1 = input
        |> List.map getCalibrationValue
        |> List.sum

    part2 = input
        |> List.map digitize
        |> List.map getCalibrationValue
        |> List.sum

    Stdout.line! "Part 1: $(Num.toStr part1)"
    Stdout.line! "Part 2: $(Num.toStr part2)"

getCalibrationValue = \str ->
    digits =
        when Grapheme.split str is
            Ok chars -> List.keepOks chars Str.toU16
            Err _ -> []

    when digits is
        [first, .., last] -> first * 10 + last
        [first] -> first * 10 + first
        [] -> crash "Invalid input"

digitize = \str ->
    str
    |> Str.replaceEach "one" "o1ne"
    |> Str.replaceEach "two" "tw2o"
    |> Str.replaceEach "three" "thr3ee"
    |> Str.replaceEach "four" "fo4ur"
    |> Str.replaceEach "five" "fi5ve"
    |> Str.replaceEach "six" "si6x"
    |> Str.replaceEach "seven" "se7ven"
    |> Str.replaceEach "eight" "ei8ght"
    |> Str.replaceEach "nine" "ni9ne"
