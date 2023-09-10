module AoC2015.Day16

open System.Text.RegularExpressions

type Aunt =
    { name: int
      children: int option
      cats: int option
      samoyeds: int option
      pomeranians: int option
      akitas: int option
      vizslas: int option
      goldfish: int option
      trees: int option
      cars: int option
      perfumes: int option }

type ScanResult =
    { children: int
      cats: int
      samoyeds: int
      pomeranians: int
      akitas: int
      vizslas: int
      goldfish: int
      trees: int
      cars: int
      perfumes: int }

let parseLine line =
    let findWithLabel label =
        let result = Regex.Match(line, sprintf "%s: (\d+)" label)
        let value = result.Groups[1].Value

        if System.String.IsNullOrEmpty value then
            None
        else
            Some(int value)

    let id =
        let result = Regex.Match(line, "\d+")
        int result.Value

    { name = id
      children = findWithLabel "children"
      cats = findWithLabel "cats"
      samoyeds = findWithLabel "samoyeds"
      pomeranians = findWithLabel "pomeranians"
      akitas = findWithLabel "akitas"
      vizslas = findWithLabel "vizslas"
      goldfish = findWithLabel "goldfish"
      trees = findWithLabel "trees"
      cars = findWithLabel "cars"
      perfumes = findWithLabel "perfumes" }

let isCandidatePart1 (scanResult: ScanResult) (aunt: Aunt) =
    let pairs =
        [| (aunt.children, scanResult.children)
           (aunt.cats, scanResult.cats)
           (aunt.samoyeds, scanResult.samoyeds)
           (aunt.pomeranians, scanResult.pomeranians)
           (aunt.akitas, scanResult.akitas)
           (aunt.vizslas, scanResult.vizslas)
           (aunt.goldfish, scanResult.goldfish)
           (aunt.trees, scanResult.trees)
           (aunt.perfumes, scanResult.perfumes)
           (aunt.cars, scanResult.cars) |]

    let matches (auntVal, scanVal) =
        match auntVal with
        | Some value -> scanVal = value
        | None -> true

    Array.forall matches pairs

let isCandidatePart2 (scanResult: ScanResult) (aunt: Aunt) =
    let pairs =
        [| (aunt.children, scanResult.children, (=))
           (aunt.cats, scanResult.cats, (>))
           (aunt.samoyeds, scanResult.samoyeds, (=))
           (aunt.pomeranians, scanResult.pomeranians, (<))
           (aunt.akitas, scanResult.akitas, (=))
           (aunt.vizslas, scanResult.vizslas, (=))
           (aunt.goldfish, scanResult.goldfish, (<))
           (aunt.trees, scanResult.trees, (>))
           (aunt.perfumes, scanResult.perfumes, (=))
           (aunt.cars, scanResult.cars, (=)) |]

    let matches (auntVal, scanVal, op) =
        match auntVal with
        | Some value -> op value scanVal
        | None -> true

    Array.forall matches pairs


let run filename =
    let input = filename |> System.IO.File.ReadAllLines |> Seq.map parseLine

    let scanResult =
        { children = 3
          cats = 7
          samoyeds = 2
          pomeranians = 3
          akitas = 0
          vizslas = 0
          goldfish = 5
          trees = 3
          cars = 2
          perfumes = 1 }

    input |> Seq.find (isCandidatePart1 scanResult) |> printfn "Part 1: %A"
    input |> Seq.find (isCandidatePart2 scanResult) |> printfn "Part 2: %A"
