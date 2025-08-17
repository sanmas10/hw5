type 'a t = S of (unit -> ('a * 'a t) option)

exception Empty

let empty = S (fun () -> None)

let cons x th = S (fun () -> Some (x, th ()))

let single x = cons x (fun () -> empty)

let next (S th) = th ()

let rec advance_until x s =
  match next s with
  | None -> empty
  | Some (x', s') ->
      if x = x' then
        s'
      else
        advance_until x s'


let is_empty s = match next s with None -> true | Some _ -> false

(* combinators *)
let rec map f s =
  match next s with
  | None -> empty
  | Some (x, s') -> cons (f x) (fun () -> map f s')


let rec accmap (acc : 'b) f (s : 'a t) =
  match next s with
  | None -> empty
  | Some (x, xs) ->
      let elt, a = f x acc in
      cons elt (fun () -> accmap a f xs)


(* list conversions *)
let rec of_list lst =
  match lst with
  | [] -> empty
  | x :: xs -> cons x (fun () -> of_list xs)


let to_list s =
  let rec loop s acc =
    match next s with
    | None -> List.rev acc
    | Some (x, xs) -> loop xs (x :: acc)
  in
  loop s []


let take_upto n s =
  let rec loop n s acc =
    if n <= 0 then
      List.rev acc
    else
      match next s with
      | None -> List.rev acc
      | Some (x, xs) -> loop (n - 1) xs (x :: acc)
  in
  loop n s []


let take_exn n s =
  let rec loop n s acc =
    if n = 0 then
      List.rev acc
    else
      match next s with
      | None -> raise Empty
      | Some (x, xs) -> loop (n - 1) xs (x :: acc)
  in
  if 0 <= n then
    loop n s []
  else
    invalid_arg "Stream.take_exn: n must be non-negative"


let take n s = try Some (take_exn n s) with Empty -> None

(* string conversions *)
let of_string s =
  let n = String.length s in
  let rec loop i =
    if i < n then
      let c = String.get s i in
      cons c (fun () -> loop (i + 1))
    else
      empty
  in
  loop 0


let to_string s =
  let buf = Buffer.create 16 in
  let rec loop s =
    match next s with
    | None -> Buffer.contents buf
    | Some (c, s) ->
        Buffer.add_char buf c;
        loop s
  in
  loop s
