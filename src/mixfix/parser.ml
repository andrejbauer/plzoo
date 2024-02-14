

(* TODO Parser combinatorji *)
(* TODO Special parser, that either fails, either takes token and makes expr and never uses-up unknown variable. *)
(*      Use this parser to create p+ and map it with seq2app *)
(*      Should be base case parser *)


module ListMonad =
struct
  let return x = [x]
  let ( let* ) xs f = List.concat_map f xs
  let fail = []
end

let wrap rs ts = List.map (fun x -> (x, ts)) rs

let rec unwrap = function
  | [] -> []
  | (r, []) :: rs -> r :: unwrap rs
  | (_, _::_) :: rs -> unwrap rs

type ('token, 'a) t = 'token list -> ('a * 'token list) list

module Monad :
sig
  val return : 'a -> ('token, 'a) t

  val ( let* ) : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t

  val ( >>= )  : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t

  val empty : 'a -> ('token, 'a) t

  val fail : ('token, 'a) t

  val get : ('token, 'token) t

  val eof : ('token, unit) t
end = struct

  let return x = fun s -> [(x, s)]

  let (>>=) (x: ('token, 'a) t) (f:'a -> ('token, 'b) t): ('token, 'b) t=
    fun s ->
      let g (x, rest) = (f x) rest in
      List.concat_map g (x s)

  let (let*) = (>>=)

  (** Parses 'empty' token *)
  let empty v = fun ts -> [(v, ts)]

  (** Fail parser directly fails. (Returns [[]]) *)
  let fail :('token, 'a) t = fun _ ->  []

  (** Gets next *)
  let get: ('token, 'token) t = function
    | [] -> []
    | t::ts -> [(t,ts)]

  let eof = function
    | [] -> [(), []]
    | _ -> []
end

let check_success = function
  | [] -> Zoo.error "could not parse"
  | [r] -> r
  | _ :: _ :: _ -> Zoo.error "ambiguous parse"

open Monad

(** If b is false, fails, otherwise parses unit *)
let check b =
  if b then
    return ()
  else
    fail

(* (\* DEBUG version *\) *)
(* let keyword k = *)
(*   let* k' = get in *)
(*   let* () = check ((Presyntax.Var k) = k') in *)
(*   print_string (" + Got keyword " ^ k ^ "\n"); return () *)
(* ;; *)

let kw k =
  let* k' = get in
  check (k = k')

(** Symmetric choice of parsers. *)
let ( ||| ) (p1:('token, 'a) t) (p2:('token, 'a) t) s =
  List.append (p1 s) (p2 s)

(** Parses zero or more occurances of p. *)

(** Concat of parsers *)
let ( @@@ ) p1 p2 =
  let* x = p1 in
  let* y = p2 in
  return (x, y)

(** Concat ignore right unit *)
let ( @@> ) p1 p2 =
  let* x = p1 in
  let* () = p2 in
  return x

(** Concat ignore left unit *)
let ( <@@ ) p1 p2 =
  let* () = p1 in
  let* x = p2 in
  return x

(** "Applicative functor apllication" *)
let (<*>) (f_par: ('token, 'a -> 'b) t) (x_par: ('token, 'a) t): ('token, 'b) t =
  let* f = f_par in
  let* x = x_par in
  return (f x)

(** Map. [f <$> p] Creates a parser that maps f over result of p *)
let (<$>) (f: 'a->'b) (x_parser: ('token, 'a) t): ('token, 'b) t =
  let* x = x_parser in
  return (f x)

(** Parse once with unit parser [p] and yield v*)
let ( >> ) p v =
let* () = p in
return v

(** Parses zero or more occurances of p. *)
let rec iter (p:('token, 'a) t):('token, 'a list) t =
  return []  |||
  (let* x = p in
  let* xs = iter p in
  return (x :: xs))

(** Parses one or more occurances of p. *)
let rec iter1 p =
  let* x = p in
  let* xs = return [] ||| iter1 p in
  return (x :: xs)

(* Že ok. Rešeno z option -> Tuki nekaj ne štima, mogoče pa vseeno rabimo explicit Error.  *)
(* Vrjetno ok? Zna biti da je option čisto odveč. Ne smemo kar vrnit empty seznama, če so še tokeni !! *)
(** Parses strings matching p between the name parts in (nonempty!) token list/vec *)
let rec between p = function
  | [] -> assert false
  | [k] -> kw k >> []
  | k :: ks ->
     kw k <@@
     (let* x = p in
      let* xs = between p ks in
      return (x :: xs))

let numeral =
  let* w = get in
  match w with
    | Presyntax.Int x -> return (Syntax.Int x)
    | _ -> fail

let wrapped c1 c2 p =
  let* () = kw c1 in
  let* x = p in
  let* () = kw c2 in
  return x

(* let parenth = wrapped "(" ")" *)

(* let html = wrapped "<" ">" ;; *)

(* let array p = *)
(*   let* x = wrapped "[" "]" (iter p) in *)
(*   return x *)

let if_then_else_endif p =
  let ite_parts = Presyntax.([Var "if"; Var "then"; Var "else"; Var "endif"]) in
  let* parts = between p ite_parts in
  match parts with
  | [c; t; e] -> return (Syntax.If (c, t, e))
  | _ -> fail

(* let data = Presyntax.([ *)
(*   Var "if"; *)
(*   Predef (Int 3); *)
(*   Var "then"; *)
(*   Predef (Int 4); *)
(*   Var "else"; *)
(*   Predef (Int 5); *)
(*   Var "endif"; *)
(* ]) *)

(* (\* let t = exec (if_then_else_endif numeral @@> eof) data *\) *)

(* let numlist = List.map (fun x -> Presyntax.Predef (Int (int_of_string x))) (String.split_on_char ' ' "1 2 3 4 5 6") *)


(* (\** A parser that parses the language, but still needs the most binding parser *\) *)
(* let language_parser big_p = ((if_then_else_endif big_p) @@> eof) *)

(* let rec debug_print_ambig = function *)
(* | [] -> () *)
(* | x::xs -> print_string (Syntax.string_of_expr x); debug_print_ambig xs *)

(* open Monad *)

let rec expr env e : Syntax.expr list =
  let open ListMonad in
  match e with

  | Presyntax.Var x ->
     if List.mem_assoc x Environment.(env.context) then
       return @@ Syntax.Var x
     else
       fail

  | Presyntax.Seq es  ->
     sequence env es

  | Presyntax.Int k ->
     return @@ Syntax.Int k

  | Presyntax.Bool b ->
     return @@ Syntax.Bool b

  | Presyntax.Nil ht ->
     return @@ Syntax.Nil ht

  | Presyntax.Times (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Times (e1, e2)

  | Presyntax.Divide (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Divide (e1, e2)

  | Presyntax.Mod (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Mod (e1, e2)

  | Presyntax.Plus (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Plus (e1, e2)

  | Presyntax.Minus (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Minus (e1, e2)

  | Presyntax.Equal (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Equal (e1, e2)

  | Presyntax.Less (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Less (e1, e2)

  | Presyntax.If (e1, e2, e3) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     let* e3 = expr env e3 in
     return @@ Syntax.If (e1, e2, e3)

  | Presyntax.Fun (x, ht, e) ->
     let* e = expr env e in
     return @@ Syntax.Fun (x, ht, e)

  | Presyntax.Pair (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Pair (e1, e2)

  | Presyntax.Fst e ->
     let* e = expr env e in
     return @@ Syntax.Fst e

  | Presyntax.Snd e ->
     let* e = expr env e in
     return @@ Syntax.Snd e

  | Presyntax.Rec (x, ht, e) ->
     let* e = expr env e in
     return @@ Syntax.Rec (x, ht, e)

  | Presyntax.Cons (e1, e2) ->
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Cons (e1, e2)

  | Presyntax.Match (e, ht, e1, x, y, e2) ->
     let* e = expr env e in
     let* e1 = expr env e1 in
     let* e2 = expr env e2 in
     return @@ Syntax.Match (e, ht, e1, x, y, e2)

(** Compute the sequence parser from the given environment *)
and sequence env es =
  let sequence_parser = get_parser env in
  unwrap @@ (sequence_parser ||| app_parser env) es

and app_parser env =
  let rec make_app =
    let open ListMonad in
    function
    | [] -> fail
    | [p] -> expr env p
    | p :: ps ->
       let* e1 = make_app ps in
       let* e2 = expr env p in
       return @@ Syntax.Apply (e1, e2)
  in
  let* p = iter get in
  wrap @@ make_app (List.rev p)

and get_parser env =
  if_then_else_endif (app_parser env)

(* let old_expr env s = *)

(*   let rec seq_p s = language_parser seq2app_p s *)

(*   and inner = function *)
(*     | Presyntax.Var varname -> Syntax.Var varname *)
(*     | Presyntax.Seq es  -> begin *)
(*         match exec seq_p es with *)
(*         | Result x -> x *)
(*         | Ambiguous x -> print_endline "Multiple parses:" ; debug_print_ambig x; failwith "Ambiguous parse" *)
(*         | Error -> failwith "Parse error" *)
(*       end *)
(*     | Presyntax.Predef x -> Syntax.from_predef inner x *)

(*   and seq2app_p = function *)
(*     | [] -> [] *)
(*     | x::y::rest -> let ex = inner x in [Syntax.Apply(ex,  inner y), rest; ex ,y::rest] (\* Try parsing 2 expr? and go forward etc. *\) *)
(*     | x::rest -> [inner x, rest] (\* Try parsing 2 expr and go forward etc. *\) *)

(* in inner s *)
