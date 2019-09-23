open Core

(* let f = DbSlow.Somelib.print_hello;; *)

(* let () = f ();; *)

(* test tokens *)
let s = "id = 3 + 1 * 2";;
let toks = String.split ~on:' ' s;;

let is_digit c =
  match c with
  | '0'..'9' -> true
  | _ -> false
;;

let is_digits s =
  String.for_all ~f:is_digit s
;;
module SumParse = struct
  type token =
  | Sum1 of token * token
  | Sum2 of string
  | Raw of string
  [@@deriving show]
  ;;

  let reduce (xs:token list) : bool * (token list) =
    match xs with
    | (Raw s)::xs when (is_digits s) -> true, (Sum2 s)::xs
    | (Sum2 _ as s1)::(Raw "*")::(Sum2 _ as s2)::xs -> true, (Sum1 (s1, s2))::xs
    | _ -> false, xs
  ;;
end

module ArithParse = struct
  type token =
  | Sum1 of token * token
  | Prod1 of token * token
  | Prod2 of string
  | Raw of string
  [@@deriving show]
  ;;

  let reduce (xs:token list) : bool * (token list) =
    match xs with
    | (Raw s)::xs when (is_digits s) -> true, (Prod2 s)::xs
    | (Prod2 _ as s1)::(Raw "*")::(Prod2 _ as s2)::xs -> true, (Prod1 (s1, s2))::xs
    | (Prod1 _ | Prod2 _ as s1)::(Raw "+")::(Prod1 _ | Prod2 _ as s2)::xs -> true, (Sum1 (s1, s2))::xs
    | _ -> false, xs
  ;;
end

module ArithStmtParse = struct
  type token =
  | Stmt of string * token
  | Sum1 of token * token
  | Prod1 of token * token
  | Prod2 of string
  | Raw of string
  [@@deriving show]
  ;;

  let reduce (xs:token list) : bool * (token list) =
    match xs with
    | (Raw s)::xs when (is_digits s) -> true, (Prod2 s)::xs
    | (Prod2 _ as s1)::(Raw "*")::(Prod2 _ as s2)::xs -> true, (Prod1 (s1, s2))::xs
    | (Prod1 _ | Prod2 _ as s1)::(Raw "+")::(Prod1 _ | Prod2 _ as s2)::xs -> true, (Sum1 (s1, s2))::xs
    | [(Sum1 _ as s);(Raw "=");(Raw id)] -> true, [Stmt (id, s)]
    | _ -> false, xs
  ;;
end

(* open SumParse *)
(* open ArithParse *)
open ArithStmtParse

let parse (toks:string list) =
  let rec parse_rec s toks =
    (* shift *)
    let s, toks = match toks with
      | t::toks -> (Raw t)::s, toks
      | _ -> s, toks
    in
    (* reduce *)
    let has_progress, s = reduce s in
    match has_progress, toks with
    | false, [] -> s
    | _ -> parse_rec s toks
  in
  parse_rec [] toks
;;

let () =
  parse toks |> List.iter ~f:(fun t -> print_endline (show_token t));;

(* shift-reduce parsing *)
(* let parse (toks:string list) (rules:rule list) = 
  let rec parse_rec s toks = 
    let tok :: toks = toks in
    Stack.push (Raw tok) 
  p_shift (Stack.create ()) toks
;; *)
