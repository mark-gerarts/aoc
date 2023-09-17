module AoC2015.Day25

let findCodeNumberAt row col =
    let mutable lastYVal = 1
    let mutable lastXVal = 1

    for y in { 1..row } do
        let yVal = lastYVal + y - 1
        lastYVal <- yVal
        lastXVal <- yVal

        for x in { 1..col } do
            let xVal = if x = 1 then lastXVal else lastXVal + x + y - 1
            lastXVal <- xVal

    lastXVal

let generateCode number =
    let rec go i prev =
        if i = number then
            prev
        else
            let next = (prev * 252533UL) % 33554393UL

            go (i + 1) next

    go 1 20151125UL

// Not 29069289
let run _ =
    findCodeNumberAt 3010 3019 |> printfn "Solution: %i"
