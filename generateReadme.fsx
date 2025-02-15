// Usage: dotnet fsi generateReadme.fsx > README.md
open System.Text.RegularExpressions
open System.IO

type Language =
    | Haskell
    | FSharp
    | Lisp
    | Lean
    | Nim
    | DuckDB
    | Roc
    | Clojure

    member this.display() =
        match this with
        | FSharp -> "F#"
        | other -> other.ToString()

    member this.shortname() =
        match this with
        | Haskell -> "hs"
        | FSharp -> "fs"
        | Lisp -> "cl"
        | Lean -> "lean"
        | Nim -> "nim"
        | DuckDB -> "duckdb"
        | Roc -> "roc"
        | Clojure -> "clj"

type Part =
    | A
    | B
    | Both

type Year = int

type Day = int

type Filepath = string

type Solution =
    { year: Year
      day: Day
      part: Part
      language: Language
      path: Filepath }

    member this.filename() =
        let parts = this.path.Split '/'
        parts[parts.Length - 1]

let firstMatchingGroup (input: string) (regex: string) =
    let matches =
        [ for m in Regex.Matches(input, regex) do
              m ]

    match matches with
    | firstMatch :: _ ->
        let groups =
            [ for g in firstMatch.Groups do
                  g ]

        match groups with
        | _ :: firstGroup :: _ -> Some firstGroup.Value
        | _ -> None
    | _ -> None

let parsePath (filepath: string) =
    let path = filepath
    let filepath = filepath.ToLower()

    let tryParseInt (number: string) =
        try
            number |> int |> Some
        with :? System.FormatException ->
            None

    let tryParseYear =
        let parts = filepath.Split "/"

        if Array.length parts < 2 then
            None
        else
            tryParseInt parts[1]

    let tryParseDay =
        firstMatchingGroup filepath "(\d+)(a|b)?\.[^.]*$"
        |> Option.map (_.TrimStart('0')) |> Option.bind tryParseInt

    let tryParseLanguage =
        let parts = filepath.Split '.'
        if Array.length parts = 0 then
            None
        else
            match parts[Array.length parts - 1] with
            | "fs" | "fsx" -> Some FSharp
            | "hs" -> Some Haskell
            | "lisp" -> Some Lisp
            | "lean" -> Some Lean
            | "sql" -> Some DuckDB
            | "nim" -> Some Nim
            | "roc" -> Some Roc
            | "clj" -> Some Clojure
            | _ -> None


    let tryParsePart =
        match firstMatchingGroup filepath "\d+(a|b)\..*" with
        | Some "a" -> Some A
        | Some "b" -> Some B
        | None -> Some Both
        | Some _ -> None

    match tryParseYear, tryParseDay, tryParsePart, tryParseLanguage with
    | Some year, Some day, Some part, Some language ->
        Some { year = year
               day = day
               part = part
               language = language
               path = path }
    | _, _, _, _ -> None

printfn "# Advent Of Code

I use Advent of Code mostly as a playground to experiment with new programming
languages.
"

let solutions =
    let options = EnumerationOptions(RecurseSubdirectories = true)
    Directory.EnumerateFiles(".", "*", options)
        |> Seq.choose parsePath
        |> Seq.map (fun s -> (s.year, s.language, s.day, s.part), s)
        |> Seq.sort
        |> Seq.map snd

// Procedural is convenient here...
for (year, solutions) in Seq.groupBy (_.year) solutions |> Seq.rev do
    printfn "## [%i](./%i)" year year
    printfn ""

    let languages = Seq.distinctBy (_.language) solutions |> Seq.map (_.language) |> Seq.sort
    printf "|"
    for language in languages do
        printf " [%s](./%i/aoc-%i-%s) |" (language.display()) year year (language.shortname())

    printfn ""

    printf "|"
    for _ in languages do
        printf " --- |"
    printfn ""

    let longestDay = solutions |> Seq.maxBy _.day |> _.day
    for day in seq {1..longestDay} do
        printf "|"
        for language in languages do
            printf " "
            let solutions = solutions |> Seq.filter (fun s -> s.language = language && s.day = day) |> Seq.toList
            match solutions with
            | [] -> printf " |"
            | [both] ->
                printf " [%s](%s) |" (both.filename()) both.path
            | [a; b] ->
                printf " [%s](%s), [%s](%s) |" (a.filename()) a.path (b.filename()) b.path
            | _ -> failwithf "Invalid match"

        printfn ""

    printfn ""

    for (day, solutions) in Seq.groupBy (_.language) solutions do
        ()

printfn "```text
                                 |
                               \ ' /
                             -- (*) --
                                >*<
                               >0<@<
                              >>>@<<*
                             >@>*<0<<<
                            >*>>@<<<@<<
                           >@>>0<<<*<<@<
                          >*>>0<<@<<<@<<<
                         >@>>*<<@<>*<<0<*<
           \*/          >0>>*<<@<>0><<*<@<<
       ___\\U//___     >*>>@><0<<*>>@><*<0<<
       |\\ | | \\|    >@>>0<*<0>>@<<0<<<*<@<<
       | \\| | _(UU)_ >((*))_>0><*<0><@<<<0<*<
       |\ \| || / //||.*.*.*.|>>@<<*<<@>><0<<<
  jgs  |\\_|_|&&_// ||*.*.*.*|_\\db//_
       \"\"\"\"|'.'.'.|~~|.*.*.*|     ____|_
           |'.'.'.|   ^^^^^^|____|>>>>>>|
           ~~~~~~~~         '\"\"\"\"`------'
```

[ASCII art source](https://asciiart.website/index.php?art=holiday/christmas/trees)"
