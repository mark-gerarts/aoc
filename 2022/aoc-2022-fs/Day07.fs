module AoC2022.Day07

open System.Text.RegularExpressions

type FileItem =
    | Dir of Dir
    | File of File

and Dir =
    { name: string
      children: list<FileItem> }

and File = { name: string; size: int }

type CwdStack = list<string>

type State = Dir * CwdStack

type Command = State -> State

let getName fileItem =
    match fileItem with
    | Dir { name = name } -> name
    | File { name = name } -> name

let rec traverseDirs fileItem =
    seq {
        match fileItem with
        | File _ -> ()
        | Dir({ children = children } as dir) ->
            yield dir

            for child in children do
                yield! (traverseDirs child)
    }

let updateCwd (updateFn: Dir -> Dir) (dir, cwdStack) : State =
    let rec updateChildren (dir: Dir) cwdStack =
        match List.tryHead cwdStack with
        | None -> updateFn dir
        | Some cwd ->
            let updateChildIfMatches child =
                match child with
                | File file -> File file
                | Dir({ name = name } as dir) when name = cwd -> Dir <| updateChildren dir (List.tail cwdStack)
                | Dir dir -> Dir dir

            { dir with children = List.map updateChildIfMatches dir.children }

    (updateChildren dir (List.rev cwdStack), cwdStack)

let mkdir name state =
    let updateFn dir =
        let newDir = Dir { name = name; children = [] }
        { dir with children = newDir :: dir.children }

    updateCwd updateFn state

let touch name size state =
    let updateFn dir =
        let newFile = File { name = name; size = size }
        { dir with children = newFile :: dir.children }

    updateCwd updateFn state

let cd name (dir, cwdStack) : State =
    match name with
    | ".." -> (dir, List.tail cwdStack)
    | "/" -> (dir, [])
    | _ -> (dir, name :: cwdStack)

let rec size fileItem =
    match fileItem with
    | File { size = size } -> size
    | Dir { children = children } -> Seq.sumBy size children

let parseLine line : Command =
    let (|FirstRegexGroup|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if (m.Success) then Some m.Groups.[1].Value else None

    match line with
    | FirstRegexGroup "\$ cd ([a-z|\/|\.\.]+)" dirName -> cd dirName
    | FirstRegexGroup "\$ ls" _ -> id
    | FirstRegexGroup "^dir (\w+)" dirName -> mkdir dirName
    | FirstRegexGroup "^\d+ [a-z\.]+" _ ->
        match line.Split ' ' with
        | [| size; filename |] -> touch filename (int size)
        | _ -> failwith $"Unexpected ls output"
    | _ -> failwith $"Unable to parse line: {line}"

let rec solveA root =
    traverseDirs (Dir root)
    |> Seq.map (size << Dir)
    |> Seq.filter (fun s -> s <= 100000)
    |> Seq.sum

let rec solveB root =
    let totalDiskSpace = 70000000
    let need = 30000000
    let currentUsedSpace = size (Dir root)
    let currentFreeSpace = totalDiskSpace - currentUsedSpace
    let minSizeToDelete = need - currentFreeSpace

    traverseDirs (Dir root)
    |> Seq.map (size << Dir)
    |> Seq.filter (fun s -> s >= minSizeToDelete)
    |> Seq.min

let run =
    let mutable state = { name = "/"; children = [] }, []
    let commands = System.IO.File.ReadLines("./input/07.txt") |> Seq.map parseLine

    for command in commands do
        state <- command state

    printfn "Part A: %i" <| solveA (fst state)
    printfn "Part B: %i" <| solveB (fst state)
