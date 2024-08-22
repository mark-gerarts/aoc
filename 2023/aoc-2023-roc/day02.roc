app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import pf.Task
import pf.File
import pf.Stdout
import parser.Core exposing [const, keep, skip, oneOf, sepBy, map]
import parser.String exposing [digits, string, parseStr]

main =
    games =
        File.readUtf8! "input/02.txt"
            |> Str.trim
            |> Str.split "\n"
            |> List.map parseGame

    part1 =
        games
        |> List.keepIf isPossible
        |> List.map .id
        |> List.sum

    part2 =
        games
        |> List.map power
        |> List.sum
    Stdout.line! "Part 1: $(Num.toStr part1)"
    Stdout.line! "Part 2: $(Num.toStr part2)"

power = \game ->
    maxOfColor = \accessor ->
        List.map game.sets accessor |> List.max |> Result.withDefault 0

    maxOfColor .red * maxOfColor .green * maxOfColor .blue

isPossible = \game ->
    List.all game.sets \set ->
        set.red <= 12 && set.green <= 13 && set.blue <= 14

getOrDefault = \dict, key, default ->
    when Dict.get dict key is
        Ok value -> value
        Err _ -> default

parseGame = \str ->
    pBlue = const Blue |> skip (string " blue")
    pRed = const Red |> skip (string " red")
    pGreen = const Green |> skip (string " green")
    pColor = oneOf [pBlue, pRed, pGreen]
    pTuple = const (\n -> \c -> (c, n)) |> keep digits |> keep pColor
    pSet =
        pTuple
        |> sepBy (string ", ")
        |> map \tuples ->
            dict = Dict.fromList tuples

            {
                blue: getOrDefault dict Blue 0,
                red: getOrDefault dict Red 0,
                green: getOrDefault dict Green 0,
            }
    pSets = pSet |> sepBy (string "; ")
    pGame =
        const (\id -> \sets -> { id, sets })
        |> skip (string "Game ")
        |> keep digits
        |> skip (string ": ")
        |> keep pSets

    when parseStr pGame str is
        Ok parsedGame -> parsedGame
        Err err -> crash (Inspect.toStr err)
