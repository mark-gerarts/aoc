module AoC2015.Day23

type Register =
    | A
    | B

type Instruction =
    | Half of Register
    | Triple of Register
    | Increment of Register
    | Jump of int
    | JumpIfEven of Register * int
    | JumpIfOne of Register * int

type Program = Instruction array

type Computer =
    { a: int
      b: int
      index: int }

    member this.getRegisterValue register =
        match register with
        | A -> this.a
        | B -> this.b

    member this.updateValue register updateFn =
        match register with
        | A -> { this with a = updateFn this.a }
        | B -> { this with b = updateFn this.b }

    member this.step() = { this with index = this.index + 1 }

    member this.jump offset =
        { this with
            index = this.index + offset }

let rec runProgram program computer =
    match Array.tryItem computer.index program with
    | None -> computer
    | Some instruction ->
        let newState =
            match instruction with
            | Half r ->
                let computer = computer.updateValue r (fun x -> x / 2)
                computer.step ()
            | Triple r ->
                let computer = computer.updateValue r (fun x -> x * 3)
                computer.step ()
            | Increment r ->
                let computer = computer.updateValue r ((+) 1)
                computer.step ()
            | Jump offset -> computer.jump offset
            | JumpIfEven(r, offset) when computer.getRegisterValue r % 2 = 0 -> computer.jump offset
            | JumpIfEven(_, _) -> computer.step ()
            | JumpIfOne(r, offset) when computer.getRegisterValue r = 1 -> computer.jump offset
            | JumpIfOne(_, _) -> computer.step ()

        runProgram program newState


let parseRegister (r: string) =
    match r[0] with
    | 'a' -> A
    | 'b' -> B
    | _ -> failwithf "Invalid register %s" r

let parseLine (line: string) =
    let parts = line.Split(' ')

    match parts[0] with
    | "hlf" -> parseRegister parts[1] |> Half
    | "tpl" -> parseRegister parts[1] |> Triple
    | "inc" -> parseRegister parts[1] |> Increment
    | "jmp" -> int parts[1] |> Jump
    | "jie" -> JumpIfEven(parseRegister parts[1], int parts[2])
    | "jio" -> JumpIfOne(parseRegister parts[1], int parts[2])
    | s -> failwithf "Invalid instruction '%s'" s

let run filename =
    let program =
        filename |> System.IO.File.ReadAllLines |> Seq.map parseLine |> Seq.toArray

    let computer = { a = 0; b = 0; index = 0 }

    let result1 = runProgram program computer
    let result2 = runProgram program { computer with a = 1 }

    printfn "Part 1: %i" (result1.b)
    printfn "Part 2: %i" (result2.b)
