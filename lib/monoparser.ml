type input = {
  text: string;
  pos: int;
}

type error = {
  desc: string;
  pos: int;
}

type 'a parser = {
  run: input -> input * ('a, error) result
}

let make_input (s: string): input = {
  text = s;
  pos = 0;
}

let make_err (s: string) (p: int): error = {
  desc = s;
  pos = p;
}

let input_sub (start: int) (till: int) (input: input): input = {
  text = String.sub input.text start till;
  pos = input.pos + start;
}

let split_input (prefix: string) (input: input):
    input * (input, error) result =
  try
    let prefix_len = String.length prefix in
      let input_len = String.length input.text in
        let input' = input_sub prefix_len (input_len - prefix_len) input in
          let pref = input_sub 0 prefix_len input in
            input', Ok pref
  with Invalid_argument _ -> (
    input,
    Error (
      make_err
      "Empty String"
      0
    )
  )

(* always succeeds *)
let res (v: 'a): 'a parser = {
  run = (fun inp -> inp, Ok v)
}

(* always fails *)
let fail (e: error): 'a parser = {
  run = (fun inp -> inp, Error e)
}

(* always returns first char *)
let char_p: string parser = {
  run =
    (fun inp ->
      let inp', c = split_input "." inp in
      match c with
      | Ok x -> inp', Ok x.text
      | Error e -> inp', Error e);
}
