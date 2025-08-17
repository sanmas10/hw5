module Token = struct
  type t =
    | LParen
    | RParen
    | Sym of string
end

module Tokenizer : sig
  val tokenize : char Stream.t -> Token.t Stream.t
end = struct
  module Tok = Token

  type state =
    | Start
    | ReadSym of Buffer.t

  let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

  let rec loop state (chars : char Stream.t) : Tok.t Stream.t =
    match Stream.next chars with
    | None -> (
        match state with
        | Start -> Stream.empty
        | ReadSym buf ->
            let sym = Buffer.contents buf in
            Stream.single (Tok.Sym sym))
    | Some (c, cs) -> (
        match (state, c) with
        | Start, ';' -> loop Start (Stream.advance_until '\n' cs)
        | Start, _ when is_space c -> loop Start cs
        | Start, '(' ->
            Stream.cons Tok.LParen (fun () -> loop Start cs)
        | Start, ')' ->
            Stream.cons Tok.RParen (fun () -> loop Start cs)
        | Start, _ ->
            (* start of symbol *)
            let buf = Buffer.create 16 in
            Buffer.add_char buf c;
            loop (ReadSym buf) cs
        | ReadSym buf, _ ->
            if is_space c || c = '(' || c = ')' then
              (* end symbol

                 Do not actually consume the current char c in this case!

                    Instead:
                      - emit this symbol token
                      - reset to the Start state
                      - recurse on **chars**

                    So we will reprocess the current char c (which is white space or
                    a paren) in the next iteration, but from the Start state instead
                    of the ReadSym state.
              *)
              let sym = Buffer.contents buf in
              Stream.cons (Tok.Sym sym) (fun () -> loop Start chars)
            else (
              Buffer.add_char buf c;
              loop state cs
            ))


  let tokenize chars = loop Start chars
end
