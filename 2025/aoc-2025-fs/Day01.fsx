#r "nuget: FsUnit,7.1.1"

open FsUnit

type Direction =
    | Left
    | Right

let parseInput =
    let parseLine (line: string) =
        match line[0], line[1..] with
        | 'L', rest -> Left, int rest
        | 'R', rest -> Right, int rest
        | _ -> failwith "Invalid input"

    System.IO.File.ReadLines "input/01.txt" |> Seq.map parseLine

let rotateOne current direction =
    match direction with
    | Left when current = 0 -> 99
    | Left -> current - 1
    | Right when current = 99 -> 0
    | Right -> current + 1

// The idea is to let rotate yield a list of all numbers we reached when
// rotating. Then part 1 needs to look at the last number of the list, and part
// 2 can count the number of zeroes in the list.
let rec rotate counter (direction, amount) =
    if amount > 0 then
        let newCounter = rotateOne counter direction
        newCounter :: rotate newCounter (direction, amount - 1)
    else
        []

let countZeros = Seq.filter ((=) 0) >> Seq.length

parseInput
|> Seq.scan (fun counter instruction -> rotate counter instruction |> Seq.last) 50
|> countZeros
|> printfn "Part 1: %i"

parseInput
|> Seq.fold
    (fun (counter, zeros) instruction ->
        let clicks = rotate counter instruction
        Seq.last clicks, zeros + countZeros clicks)
    (50, 0)
|> snd
|> printfn "Part 2: %i"

rotate 11 (Right, 8) |> should equal [ 12..19 ]
rotate 19 (Left, 19) |> should equal [ 18..-1..0 ]
rotate 0 (Left, 1) |> should equal [ 99 ]
rotate 99 (Right, 1) |> should equal [ 0 ]
rotate 5 (Left, 10) |> should equal ([ 4..-1..0 ] @ [ 99..-1..95 ])
rotate 95 (Right, 5) |> should equal [ 96; 97; 98; 99; 0 ]

rotate 50 (Left, 201)
|> should equal ([ 49..-1..0 ] @ [ 99..-1..0 ] @ [ 99..-1..49 ])
