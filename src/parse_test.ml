open Trefoil3lib
open Errors
module B = Syntax.Binding
module E = Syntax.Expr
module V = Syntax.Val

let%test _ = Run.expr_of_string "3" = E.Val (V.Int 3)

let%test _ = Run.expr_of_string "-10" = E.Val (V.Int (-10))

let%test "parsing_false" =
  Run.expr_of_string "false" = E.Val (V.Bool false)


let%test "test_add_abstract_syntax_error" =
  try
    ignore (Run.expr_of_string "(+ 1)");
    false
  with
  | AbstractSyntaxError _ -> true


(* Here is a template for a psrsing test for let expressions *)
(* Parsing test for v3 let: (let ((x 3)) (+ x 1)) **)
let%test _ =
  let parsed_let = Run.expr_of_string "(let ((x 3)) (+ x 1))" in
  let manually_constructed_let =
    E.Let ( [ ("x", E.Val (V.Int 3)) ],
      E.Add (E.Var "x", E.Val (V.Int 1)) )
  in
  parsed_let = manually_constructed_let

(* Malformed let: duplicate variable names in the same let *)
let%test _ =
  try
    ignore (Run.expr_of_string "(let ((x 1) (x 2)) x)");
    false
  with
  | AbstractSyntaxError _ -> true

let%test "test binding parsing" =
  let parsed_test =
    Run.binding_of_string "(test (= (cons 1 nil) (cons 1 nil)))"
  in
  let manually_constructed_test =
    B.Test
      (E.Eq ( E.Cons (E.Val (V.Int 1), E.Val V.Nil),
        E.Cons (E.Val (V.Int 1), E.Val V.Nil)))
  in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try
    ignore (Run.binding_of_string "(test 1 2)");
    false
  with
  | AbstractSyntaxError _ -> true
