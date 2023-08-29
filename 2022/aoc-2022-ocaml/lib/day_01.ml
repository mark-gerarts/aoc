type dbg = string list [@@deriving show]

let run input_filename =
  let input = In_channel.with_open_bin input_filename In_channel.input_all in
  print_endline input
