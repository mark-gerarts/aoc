module AoC2023.Day01

let replaceDigitWords (line: string) =
    // Replacements are like "on1ne" to still keep overlaps.
    line
        .Replace("one", "on1ne")
        .Replace("two", "tw2wo")
        .Replace("three", "th3ree")
        .Replace("four", "fo4ur")
        .Replace("five", "fi5ve")
        .Replace("six", "si6x")
        .Replace("seven", "se7ven")
        .Replace("eight", "ei8ght")
        .Replace("nine", "ni9ne")

let firstDigit line =
    line |> Seq.find System.Char.IsDigit |> (string >> int)

let solve lines =
    let leftSum = lines |> Seq.sumBy firstDigit
    let rightSum = lines |> Seq.sumBy (Seq.rev >> firstDigit)

    leftSum * 10 + rightSum

let run filename =
    let lines = filename |> System.IO.File.ReadAllLines

    lines |> solve |> printfn "Part 1: %i"
    lines |> Seq.map replaceDigitWords |> solve |> printfn "Part 2: %i"
