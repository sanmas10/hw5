module Val = struct
  type t =
    | Bool of bool
    | Int of int
    | Symbol of string
    | Nil
    | Cons of t * t

  let rec to_string v =
    match v with
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Symbol s -> "'" ^ s
    | Nil -> "nil"
    | Cons (v1, v2) ->
        Printf.sprintf "(%s, %s)" (to_string v1) (to_string v2)
end

module Expr = struct
  type t =
    | Val of Val.t
    | Var of string
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Eq of t * t
    | If of t * t * t
    | Cons of t * t
    | IsNil of t
    | IsCons of t
    | Car of t
    | Cdr of t
    | Print of t
    | Let of (string * t) list * t
    | Call of string * t list
    | Cond of (t * t) list

  let rec to_string e =
    match e with
    | Val v -> Val.to_string v
    | Var x -> Printf.sprintf "Var(%s)" x
    | Add (e1, e2) ->
        Printf.sprintf "Add(%s, %s)" (to_string e1) (to_string e2)
    | Sub (e1, e2) ->
        Printf.sprintf "Sub(%s, %s)" (to_string e1) (to_string e2)
    | Mul (e1, e2) ->
        Printf.sprintf "Mul(%s, %s)" (to_string e1) (to_string e2)
    | Eq (e1, e2) ->
        Printf.sprintf "Eq(%s, %s)" (to_string e1) (to_string e2)
    | If (e1, e2, e3) ->
        Printf.sprintf "If(%s, %s, %s)" (to_string e1) (to_string e2)
          (to_string e3)
    | Cons (e1, e2) ->
        Printf.sprintf "Cons(%s, %s)" (to_string e1) (to_string e2)
    | IsNil e -> Printf.sprintf "IsNil(%s)" (to_string e)
    | IsCons e -> Printf.sprintf "IsCons(%s)" (to_string e)
    | Car e -> Printf.sprintf "Car(%s)" (to_string e)
    | Cdr e -> Printf.sprintf "Cdr(%s)" (to_string e)
    | Print e -> Printf.sprintf "Print(%s)" (to_string e)
    | Let (bindings, body) ->
        let bindings =
          List.map
            (fun (name, expr) ->
              Printf.sprintf "(%s %s)" name (to_string expr))
            bindings
        in
        Printf.sprintf "Let((%s) %s)"
          (String.concat " " bindings)
          (to_string body)
    | Call (f, args) ->
        Printf.sprintf "Call(%s %s)" f
          (String.concat " " (List.map to_string args))
    | Cond clauses ->
        Printf.sprintf "Cond(%s)"
          (String.concat " "
             (List.map
                (fun (lhs, rhs) ->
                  Printf.sprintf "(%s %s)" (to_string lhs)
                    (to_string rhs))
                clauses))
end

module Binding = struct
  type fn_info =
    { name : string
    ; param_names : string list
    ; body : Expr.t
    }

  type t =
    | Var of string * Expr.t
    | Test of Expr.t
    | Expr of Expr.t
    | Fn of fn_info

  let to_string b =
    match b with
    | Var (s, e) ->
        Printf.sprintf "(define %s %s)" s (Expr.to_string e)
    | Test e -> Printf.sprintf "(test %s)" (Expr.to_string e)
    | Expr e -> Expr.to_string e
    | Fn { name; param_names; body } ->
        Printf.sprintf "(define (%s %s) %s)" name
          (String.concat " " param_names)
          (Expr.to_string body)
end
