module StrMap = Map.Make (String)

type value = Syntax.Val.t

type fn_info = Syntax.Binding.fn_info

type entry =
  | VarEntry of value
  | FnEntry of fn_info * t

and t = entry StrMap.t

let empty : t = StrMap.empty

let bind : string -> entry -> t -> t = StrMap.add

let lkup : string -> t -> entry option = StrMap.find_opt

let rec bind_all bindings env =
  match bindings with
  | [] -> env
  | (x, v) :: tl -> bind_all tl (bind x v env)


let to_string (value_to_string : entry -> string) (map : t) : string =
  let bindings = StrMap.bindings map in
  let binding_strings =
    List.map
      (fun (k, v) -> Printf.sprintf "%s: %s" k (value_to_string v))
      bindings
  in
  "{ " ^ String.concat "; " binding_strings ^ " }"
