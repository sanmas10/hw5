open Errors

module PST = struct
  type t =
    | Atom of string
    | Node of t list

  let to_string pst =
    let rec loop buf pst =
      match pst with
      | Atom s -> Buffer.add_string buf s
      | Node [] -> Buffer.add_string buf "()"
      | Node (x :: xs) ->
          Buffer.add_char buf '(';
          loop buf x;
          let rec list_loop xs =
            match xs with
            | [] -> ()
            | x :: xs ->
                Buffer.add_char buf ' ';
                loop buf x;
                list_loop xs
          in
          list_loop xs;
          Buffer.add_char buf ')'
    in
    let buf = Buffer.create 16 in
    loop buf pst;
    Buffer.contents buf
end

module PSTParser : sig
  module Tok = Lex.Token

  val parse : Tok.t Stream.t -> PST.t Stream.t
end = struct
  module Tok = Lex.Token

  type state =
    | Start
    | ReadList of PST.t list * state

  let rec loop state tokens =
    match Stream.next tokens with
    | None -> (
        match state with
        | Start -> Stream.empty
        | ReadList (_, _) ->
            failwith "PSTParser: unexpected end of input")
    | Some (tok, toks) -> (
        match (state, tok) with
        | Start, Tok.LParen ->
            let st = ReadList ([], Start) in
            loop st toks
        | Start, Tok.RParen -> failwith "PSTParser: Unexpected ')' "
        | Start, Tok.Sym t ->
            Stream.cons (PST.Atom t) (fun () -> loop Start toks)
        | ReadList (psts, paused), Tok.LParen ->
            (* start reading a new sublist *)
            let st = ReadList ([], ReadList (psts, paused)) in
            loop st toks
        | ReadList (psts, Start), Tok.RParen ->
            (* finished with all sublists *)
            let pst = PST.Node (List.rev psts) in
            Stream.cons pst (fun () -> loop Start toks)
        | ReadList (psts_1, ReadList (psts_2, paused)), Tok.RParen ->
            (* finished a sublist, resume previous paused state *)
            let pst = PST.Node (List.rev psts_1) in
            let st = ReadList (pst :: psts_2, paused) in
            loop st toks
        | ReadList (psts, paused), Tok.Sym t ->
            (* read an atom onto the current list *)
            let st = ReadList (PST.Atom t :: psts, paused) in
            loop st toks)


  let parse (tokens : Tok.t Stream.t) : PST.t Stream.t =
    loop Start tokens
end

module Expr = struct
  module E = Syntax.Expr
  module V = Syntax.Val

  let of_pst pst =
    let rec loop pst =
      match pst with
      | PST.Atom s -> (
          try E.Val (V.Int (int_of_string s)) with
          | Failure _ -> (
            match s with
            | "true" -> E.Val (V.Bool true)
            (* TODO: add cases for other keywords here *)
            | "false" -> E.Val (V.Bool false)
            | "nil" -> E.Val V.Nil
            | _ when String.length s > 0 && s.[0] = '\'' ->
                E.Val (V.Symbol (String.sub s 1 (String.length s - 1)))
            | _ -> E.Var s))
      | PST.Node [] ->
          raise
            (AbstractSyntaxError "Expected expression but got '()'")
      | PST.Node (hd :: args) -> (
          match (hd, args) with
          | PST.Node _, _ ->
              raise
                (AbstractSyntaxError
                   ("Expressions must start with a symbol, but got "
                   ^ PST.to_string hd))
          | PST.Atom "+", [ left; right ] ->
              E.Add (loop left, loop right)
          | PST.Atom "+", _ ->
              raise
                (AbstractSyntaxError
                   ("operator + expects 2 args but got "
                   ^ PST.to_string pst))
          (* parsing for -, *, and = operators *)
          | PST.Atom "-", [ left; right ] -> 
              E.Sub (loop left, loop right)
          | PST.Atom "-", _ ->
              raise
                (AbstractSyntaxError
                   ("operator - expects 2 args but got " ^ PST.to_string pst))
          | PST.Atom "*", [ left; right ] -> E.Mul (loop left, loop right)
          | PST.Atom "*", _ ->
              raise
                (AbstractSyntaxError
                   ("operator * expects 2 args but got " ^ PST.to_string pst))
          | PST.Atom "=", [ left; right ] -> E.Eq (loop left, loop right)
          | PST.Atom "=", _ ->
              raise
                (AbstractSyntaxError
                   ("operator = expects 2 args but got " ^ PST.to_string pst))
          | PST.Atom "if", [ prd; thn; els ] ->
              E.If (loop prd, loop thn, loop els)
          | PST.Atom "if", _ ->
              raise
                (AbstractSyntaxError
                   ("'if' expression expects 3 args but got "
                   ^ PST.to_string pst))
          (* list and pair functions *)
          | PST.Atom "cons", [ e1; e2 ] -> E.Cons (loop e1, loop e2)
          | PST.Atom "cons", _ ->
              raise
                (AbstractSyntaxError
                   ("operator cons expects 2 args but got " ^ PST.to_string pst))
          | PST.Atom "car", [ e ] -> E.Car (loop e)
          | PST.Atom "car", _ ->
              raise
                (AbstractSyntaxError
                   ("operator car expects 1 arg but got " ^ PST.to_string pst))
          | PST.Atom "cdr", [ e ] -> E.Cdr (loop e)
          | PST.Atom "cdr", _ ->
              raise
                (AbstractSyntaxError
                   ("operator cdr expects 1 arg but got " ^ PST.to_string pst))
          | PST.Atom "nil?", [ e ] -> E.IsNil (loop e)
          | PST.Atom "nil?", _ ->
              raise
                (AbstractSyntaxError
                   ("operator nil? expects 1 arg but got " ^ PST.to_string pst))
          | PST.Atom "cons?", [ e ] -> E.IsCons (loop e)
          | PST.Atom "cons?", _ ->
              raise
                (AbstractSyntaxError
                   ("operator cons? expects 1 arg but got " ^ PST.to_string pst)) 
          (* parsing for let expressions *)
          (* v3 let: (let ((x e1) (y e2) ...) body) ; zero or more bindings allowed *)
          | PST.Atom "let", [ PST.Node binds; body ] ->
              let parse_binding = function
                | PST.Node [ PST.Atom name; e ] -> (name, loop e)
                | _ -> raise (AbstractSyntaxError "Malformed let expression")
              in
              let bindings = List.map parse_binding binds in
              (* optional: reject duplicate names *)
              let () =
                let names = List.map fst bindings in
                let rec check seen = function
                  | [] -> ()
                  | x :: xs ->
                      if List.mem x seen then
                        raise (AbstractSyntaxError "Malformed let expression")
                      else check (x :: seen) xs
                in
                check [] names
              in
              E.Let (bindings, loop body)
          | PST.Atom "let", _ ->
              raise (AbstractSyntaxError "Malformed let expression")
          (* cond: (cond (p1 b1) (p2 b2) ...) *)
          | PST.Atom "cond", clauses ->
              let parse_clause = function
                | PST.Node [p; b] -> (loop p, loop b)
                | _ -> raise (AbstractSyntaxError "Malformed cond expression")
              in
              let pairs = List.map parse_clause clauses in
              E.Cond pairs

          (* print: (print e) *)
          | PST.Atom "print", [ e ] -> E.Print (loop e)
          | PST.Atom "print", _ ->
              raise (AbstractSyntaxError ("print expects 1 arg but got " ^ PST.to_string pst))
          (* function call fallback: (fname arg1 arg2 ...) *)
          | PST.Atom fname, args ->
              let args' = List.map loop args in
              E.Call (fname, args')
      )
    in
    loop pst

  let parse = Stream.map of_pst
end

module Binding = struct
  module Binding = Syntax.Binding

  let of_pst pst =
    match pst with
    | PST.Atom _ -> Binding.Expr (Expr.of_pst pst)
    | PST.Node [] ->
      raise (AbstractSyntaxError "Expected binding but got '()'")
    | PST.Node (hd :: args) -> (
      match (hd, args) with
      (* function binding: (define (f a1 ... an) body) *)
      | PST.Atom "define", [ PST.Node (PST.Atom fname :: params); body ] ->
          let params =
            List.map (function
              | PST.Atom p -> p
              | _ -> raise (AbstractSyntaxError "malformed define")
            ) params
          in
          (* optional: reject duplicate params *)
          let () =
            let rec check seen = function
              | [] -> ()
              | x :: xs ->
                  if List.mem x seen then
                    raise (AbstractSyntaxError "malformed define")
                  else check (x :: seen) xs
            in
            check [] params
          in
          Binding.Fn { name = fname; param_names = params; body = Expr.of_pst body }

      (* variable binding: (define x e) *)
      | PST.Atom "define", [ PST.Atom var; rhs ] ->
          Binding.Var (var, Expr.of_pst rhs)


        | PST.Atom "define", _ -> 
          raise (AbstractSyntaxError "malformed define")
        (* TODO: parse test bindings here *)
        | PST.Atom "test", [ e ] -> Binding.Test (Expr.of_pst e)
        | PST.Atom "test", _ ->
          raise (AbstractSyntaxError "Malformed test binding: expected one argument")
        | _ -> Binding.Expr (Expr.of_pst pst))


  let parse = Stream.map of_pst
end