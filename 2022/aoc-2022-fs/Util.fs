module AoC2022.Util

// @see https://github.com/nathanleiby/advent-of-code-2022/blob/main/03/Program.fs#L3
let private tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value

let tee (value: 'T) = tap (fun x -> printfn "%A" x) value
