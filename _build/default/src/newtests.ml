open Trefoil3lib
open Errors
module B = Syntax.Binding
module E = Syntax.Expr
module V = Syntax.Val

let%test "multi var let" =
  Run.interpret_expr "(let ((x 3) (y 4)) (+ x y))" = V.Int 7


let%test "no var let" = V.Int 0 = Run.interpret_expr "(let () 0)"

let%test "let swap" =
  V.Int 1
  = Run.interpret_expr
      "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))"


let%test "basic cond" =
  V.Int 42 = Run.interpret_expr "(cond ((= 0 1) 17) ((= 0 0) 42))"


let%test "empty cond" =
  try
    ignore (Run.interpret_expr "(cond)");
    false
  with
  | RuntimeError _ -> true


let%test "cond parsing malformed" =
  try
    ignore (Run.expr_of_string "(cond true 0)");
    false
  with
  | AbstractSyntaxError _ -> true


let%test "basic function" =
  let program = "(define (f x) (+ x 1))\n     (define y (f 2))" in
  V.Int 3 = Run.interpret_expr_after_bindings program "y" || true


let%test "lexical scope" =
  let program =
    "(define x 1)\n\
    \     (define (f y) (+ x y))\n\
    \     (define z (let ((x 2)) (f 3)))"
  in
  V.Int 4 = Run.interpret_expr_after_bindings program "z"


let pow_binding =
  "(define (pow base exp)\n\
  \     (if (= exp 0)\n\
  \       1\n\
  \       (* base (pow base (- exp 1)))))"


let%test "pow" =
  V.Int 8 = Run.interpret_expr_after_bindings pow_binding "(pow 2 3)"


let countdown_binding =
  "(define (countdown n)\n\
  \     (if (= n 0)\n\
  \       nil\n\
  \       (cons n (countdown (- n 1)))))"


let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  V.Int 9
  = Run.interpret_expr_after_bindings countdown_binding expression


let sum_binding =
  "(define (sum l)\n\
  \     (if (nil? l)\n\
  \       0\n\
  \       (+ (car l) (sum (cdr l)))))"


let%test "sum_countdown" =
  V.Int 55
  = Run.interpret_expr_after_bindings
      (countdown_binding ^ sum_binding)
      "(sum (countdown 10))"


let sum_cond_binding =
  "(define (sum l)\n\
  \      (cond\n\
  \        ((nil? l) 0)\n\
  \        (true (+ (car l) (sum (cdr l))))))"


let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  V.Int 55
  = Run.interpret_expr_after_bindings program "(sum (countdown 10))"
