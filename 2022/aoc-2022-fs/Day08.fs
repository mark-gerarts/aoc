module AoC2022.Day08

type Direction =
    | Up
    | Down
    | Left
    | Right

let allDirections = [ Up; Down; Left; Right ]

let charToDigit c = int c - int '0'

let lookAt map direction =
    match direction with
    | Right -> map
    | Left -> Seq.map Seq.rev map
    | Down -> Seq.transpose map
    | Up -> Seq.map Seq.rev (Seq.transpose map)

let parseMap =
    let input = System.IO.File.ReadAllText("./input/08.sample.txt").Trim()
    let lines = input.Split '\n'
    let width = input.Split '\n' |> Array.head |> Seq.length

    String.concat "" lines
    |> Seq.map charToDigit
    |> Seq.zip (Seq.initInfinite id)
    |> Seq.chunkBySize width
    |> Seq.map Seq.ofArray

let countVisible map =
    let rec collectVisibleFromLine visible line =
        let visibleHeights = List.map snd visible

        let heighest =
            match visibleHeights with
            | [] -> -1
            | _ -> List.max visibleHeights

        match line with
        | [] -> List.map fst visible
        | (id, height) :: xs when height > heighest -> collectVisibleFromLine ((id, height) :: visible) xs
        | _ :: xs -> collectVisibleFromLine visible xs

    let collectVisibleFromMap map =
        map
        |> Seq.map (List.ofSeq >> collectVisibleFromLine [] >> Set.ofList)
        |> Set.unionMany

    allDirections
    |> Seq.map (lookAt map)
    |> Seq.map collectVisibleFromMap
    |> Set.unionMany
    |> Set.count

let run =
    let map = parseMap

    printfn "Part A: %i" <| countVisible map
