open Errors

module Expr = struct
  module V = Syntax.Val
  module E = Syntax.Expr

  let expect_int v =
    match v with
    | V.Int i -> i
    | _ -> raise (RuntimeError ("Expected int, got " ^ V.to_string v))

  let expect_bool v =
    match v with
    | V.Bool b -> b
    | _ ->
      raise (RuntimeError ("Expected bool, got " ^ V.to_string v))

  let rec eq_val (a : V.t) (b : V.t) : bool =
  match (a, b) with
  | V.Int i1, V.Int i2 -> i1 = i2
  | V.Bool b1, V.Bool b2 -> b1 = b2
  | V.Symbol s1, V.Symbol s2 -> String.equal s1 s2
  | V.Nil, V.Nil -> true
  | V.Cons (h1, t1), V.Cons (h2, t2) -> eq_val h1 h2 && eq_val t1 t2
  | _ -> false

  let rec interp_with_env env e =
    match e with
    | E.Val v -> v
    | E.Var x -> (
    match Env.lkup x env with
    | None -> raise (RuntimeError ("Unbound name " ^ x))
    | Some (Env.VarEntry v) -> v
    | Some (Env.FnEntry _) ->
      raise (RuntimeError ("Expected variable, found function: " ^ x)) )
    | E.Add (e1, e2) ->
      let v1 = interp_with_env env e1 in
      let v2 = interp_with_env env e2 in
      V.Int (expect_int v1 + expect_int v2)
    (* TODO: implement other cases here *)
    (* evaluation logic for -, *, and = *)
    | E.Sub (e1, e2) ->
      let v1 = interp_with_env env e1 in
      let v2 = interp_with_env env e2 in
      V.Int (expect_int v1 - expect_int v2)
    | E.Mul (e1, e2) ->
      let v1 = interp_with_env env e1 in
      let v2 = interp_with_env env e2 in
      V.Int (expect_int v1 * expect_int v2)
    | E.Eq (e1, e2) ->
      let v1 = interp_with_env env e1 in
      let v2 = interp_with_env env e2 in
      V.Bool (eq_val v1 v2)
    (* evaluation logic for if expressions *)
    | E.If (e_pred, e_then, e_else) -> (
      let v_pred = interp_with_env env e_pred in
      match v_pred with
      | V.Bool false -> interp_with_env env e_else
      | _ -> interp_with_env env e_then )
    (* evaluation logic for let expressions *)
    (* NEW — add this instead *)
  | E.Let (bindings, body) ->
      (* evaluate each rhs in the ORIGINAL env *)
      let evaluated : (string * V.t) list =
        List.map (fun (x, e) -> (x, interp_with_env env e)) bindings
      in
      (* extend env with VarEntry for each binding *)
      let env' =
        List.fold_left
          (fun acc (x, v) -> Env.bind x (Env.VarEntry v) acc)
          env
          evaluated
      in
      interp_with_env env' body
    (* evaluation logic for list/pair functions *)
    | E.Cons (e1, e2) ->
      let v1 = interp_with_env env e1 in
      let v2 = interp_with_env env e2 in
      V.Cons (v1, v2)
    | E.Car e -> (
      let v = interp_with_env env e in
      match v with
      | V.Cons (v1, _) -> v1
      | _ -> raise (RuntimeError ("car expects a cons pair, but got " ^ V.to_string v)))
    | E.Cdr e -> (
      let v = interp_with_env env e in
      match v with
      | V.Cons (_, v2) -> v2
      | _ -> raise (RuntimeError ("cdr expects a cons pair, but got " ^ V.to_string v)))
    | E.IsNil e ->
        let v = interp_with_env env e in
        V.Bool (v = V.Nil)
    | E.IsCons e -> (
      let v = interp_with_env env e in
      match v with
      | V.Cons _ -> V.Bool true
      | _ -> V.Bool false)
    | E.Print e ->
      let v = interp_with_env env e in
      print_endline (V.to_string v);
      V.Nil
    | E.Cond clauses ->
      let rec step = function
        | [] -> raise (RuntimeError "cond: no predicate was true")
        | (p, b) :: rest ->
          let vp = interp_with_env env p in
          match vp with
          | V.Bool false -> step rest
          | _ -> interp_with_env env b
      in
      step clauses
    | E.Call (fname, args) -> (
      match Env.lkup fname env with
      | None ->
        raise (RuntimeError ("Unbound function " ^ fname))
      | Some (Env.VarEntry _) ->
        raise (RuntimeError ("Called non-function: " ^ fname))
      | Some (Env.FnEntry (info, def_env)) ->
        (* arity check *)
        let params = info.param_names in
        if List.length params <> List.length args
        then raise (RuntimeError "arity mismatch");

        (* evaluate args left-to-right in the CALL env *)
        let arg_vals = List.map (interp_with_env env) args in

        (* re-inject the current function binding into the DEFINING env to support recursion *)
        let def_env_with_self =
          match Env.lkup info.name env with
          | Some self_binding -> Env.bind info.name self_binding def_env
          | None ->
            (* fallback: if somehow not present in the call env, bind a closure anyway *)
            Env.bind info.name (Env.FnEntry (info, def_env)) def_env
        in
        (* extend with params *)
        let call_env =
          List.fold_left2
            (fun acc p v -> Env.bind p (Env.VarEntry v) acc)
            def_env_with_self params arg_vals
        in
        interp_with_env call_env info.body
    )

  let interpret (e : E.t) : V.t = interp_with_env Env.empty e
end

module Binding = struct
  module B = Syntax.Binding
  module V = Syntax.Val

  let interpret env binding =
    match binding with
    | B.Var (x, e) ->
      let v = Expr.interp_with_env env e in
      Printf.printf "%s = %s\n" x (V.to_string v);
      Env.bind x (Env.VarEntry v) env
    | B.Fn info ->
      (* bind the function name to a closure that captures the current env;
        recursion will be handled at call time by re-injecting the self binding *)
      let closure = Env.FnEntry (info, env) in
      Env.bind info.name closure env
    | B.Expr e ->
      let v = Expr.interp_with_env env e in
      print_endline (V.to_string v);
      env
    (* TODO: implement test bindings here *)
    (* evaluation logic for test bindings *)
    | B.Test e -> (
      let v = Expr.interp_with_env env e in
      match v with
      | V.Bool true -> env
      | _ ->
          raise (RuntimeError ("Test failed: expression evaluated to " ^ V.to_string v))
      )

  let interpret_bindings = List.fold_left interpret
end