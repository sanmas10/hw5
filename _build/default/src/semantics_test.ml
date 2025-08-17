open Trefoil3lib
open Errors
module B = Syntax.Binding
module E = Syntax.Expr
module V = Syntax.Val

let%test _ = Run.interpret_expr "3" = V.Int 3

let%test _ = Run.interpret_expr "-10" = V.Int (-10)

let%test "interpret_true" = Run.interpret_expr "true" = V.Bool true

let%test "interpret_false" = Run.interpret_expr "false" = V.Bool false

let xto3 = Env.bind "x" (Env.VarEntry (V.Int 3)) Env.empty


let%test _ = Semantics.Expr.interp_with_env xto3 (E.Var "x") = V.Int 3

let%test _ =
  try
    ignore (Semantics.Expr.interp_with_env xto3 (E.Var "y"));
    false
  with
  | RuntimeError _ -> true

let%test _ = Run.interpret_expr "(+ 1 2)" = V.Int 3

let%test "test_add_wrong_types" =
  try
    ignore (Run.interpret_expr "(+ 1 true)");
    false
  with
  | RuntimeError _ -> true

let%test "interpret_sub" = Run.interpret_expr "(- 1 2)" = V.Int (-1)

let%test "interpret_mul" = Run.interpret_expr "(* 2 3)" = V.Int 6

let%test _ = Run.interpret_expr "(= 3 (+ 1 2))" = V.Bool true

let%test _ = Run.interpret_expr "(= 4 (+ 1 2))" = V.Bool false

let%test "eq_mixed_types_is_false" =
  Run.interpret_expr "(= 4 true)" = V.Bool false

let%test _ = Run.interpret_expr "(if true 0 1)" = V.Int 0

let%test _ = Run.interpret_expr "(if false 0 1)" = V.Int 1

let%test _ = Run.interpret_expr "(if true 0 x)" = V.Int 0

let%test _ = Run.interpret_expr "(if 5 0 1)" = V.Int 0

let%test "test let1" =
  Run.interpret_expr "(let ((x 3)) (+  x 1))" = V.Int 4


let%test "test let2" =
  Run.interpret_expr "(let ((x 1)) (let ((x 2)) x))" = V.Int 2


let%test "test let3" =
  Run.interpret_expr "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))"
  = V.Int 21


let%test _ = Run.interpret_expr "(+ ; asdf asdf asdf \n1 2)" = V.Int 3

let%test _ = Run.interpret_expr "nil" = V.Nil

let%test _ =
  Run.interpret_expr "(cons 1 2)" = V.Cons (V.Int 1, V.Int 2)


let%test _ = Run.interpret_expr "(car (cons 1 2))" = V.Int 1

let%test _ = Run.interpret_expr "(cdr (cons 1 2))" = V.Int 2

let%test _ =
  Run.interpret_expr_after_bindings "(define x (+ 1 2))" "x" = V.Int 3


(* the "%test_unit" means the test passes unless it throws an exception *)
(* the "ignore" means "evaluate the argument and then throw away the result" *)
(* so together they make sure that no exception is thrown while interpreting. *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  ignore (Run.interpret_bindings program)


let%test "failing test binding" =
  try
    ignore (Run.interpret_bindings "(define x 3) (test (= 2 x))");
    false
  with
  | RuntimeError _ -> true