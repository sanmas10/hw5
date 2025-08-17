open Trefoil3lib

let read_until_end_marker () =
  let buffer = Buffer.create 128 in

  let rec loop () =
    flush stdout;
    match read_line () with
    | exception End_of_file -> raise End_of_file
    | line ->
        if String.ends_with line ~suffix:";;" then (
          Buffer.add_string buffer line;
          Buffer.contents buffer
        ) else (
          Buffer.add_string buffer line;
          Buffer.add_char buffer '\n';
          loop ()
        )
  in
  print_string "> ";
  loop ()


let rec repl env =
  try
    let bindings =
      Stream.of_string (read_until_end_marker ())
      |> Lex.Tokenizer.tokenize
      |> Parse.PSTParser.parse
      |> Parse.Binding.parse
      |> Stream.to_list
    in
    let env' = Semantics.Binding.interpret_bindings env bindings in
    repl env'
  with
  | End_of_file -> print_endline "\nExiting."
  | exn ->
      Printf.printf "Error: %s\n%!" (Printexc.to_string exn);
      repl env


let _ = repl Env.empty
