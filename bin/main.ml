open Monoparser

let show_input (input : input) : string =
  Printf.sprintf
  "{\n\ttext: %s,\n\tpos: %d,\n}"
  input.text
  input.pos

let show_err (err : error) : string =
  Printf.sprintf
  "Error(pos: %d): %s"
  err.pos
  err.desc

let () =
  "0123456789"
  |> make_input
  |> input_sub 0 1
  |> show_input
  |> print_endline;

  "0123456789"
  |> make_input
  |> input_sub 1 9
  |> show_input
  |> print_endline;

  let inp', c = "" |> make_input |> char_p.run in
    inp' |> show_input |> print_endline;
    match c with
    | Ok c -> print_endline c
    | Error e -> e |> show_err |> print_endline
