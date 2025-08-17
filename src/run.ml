let pst_stream_of_string s =
  s
  |> Stream.of_string
  |> Lex.Tokenizer.tokenize
  |> Parse.PSTParser.parse


let pst_of_string s =
  s |> pst_stream_of_string |> Stream.to_list |> List.hd


let interpret_bindings str =
  str
  |> pst_stream_of_string
  |> Parse.Binding.parse
  |> Stream.to_list
  |> Semantics.Binding.interpret_bindings Env.empty


let expr_of_string s = s |> pst_of_string |> Parse.Expr.of_pst

let binding_of_string s = s |> pst_of_string |> Parse.Binding.of_pst

let bindings_of_string s =
  s |> pst_stream_of_string |> Parse.Binding.parse |> Stream.to_list


let interpret_expr s = s |> expr_of_string |> Semantics.Expr.interpret

let interpret_expr_after_bindings bs e =
  let env = interpret_bindings bs in
  Semantics.Expr.interp_with_env env (expr_of_string e)
