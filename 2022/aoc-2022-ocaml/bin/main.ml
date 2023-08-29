let () =
  match Sys.argv with
  | [| _; day; input_filename |] -> (
    match day with
    | "01"
    | "1" -> Aoc2022.Day_01.run input_filename
    | _ -> failwith "Incorrect day given")
  | _ -> failwith "Not enough args!"
