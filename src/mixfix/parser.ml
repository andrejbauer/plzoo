

(* TODO Parser combinatorji *)
(* TODO Special parser, that either fails, either takes token and makes expr and never uses-up unknown variable. *)
(*      Use this parser to create p+ and map it with seq2app *)
(*      Should be base case parser *)

type ('token, 'a) parser = 'token list -> ('a * 'token list) list
type ('token, 'a) t =  ('token, 'a) parser
type 'a parser_result = Error | Result of 'a | Ambiguous of 'a list

module Monad :
sig
  val return : 'a -> ('token, 'a) parser
  val ( let* ) : ('token, 'a) parser -> ('a -> ('token, 'b) parser) -> ('token, 'b) parser

  val empty : 'a -> ('token, 'a) parser

  val fail : ('token, 'a) parser

  val get : ('token, 'token) parser

  val eof : ('token, unit) parser

  val exec : ('token, 'a) parser -> 'token list -> 'a parser_result
end = struct
  

  let return x = fun s -> [(x, s)]

  let (>>=) (x: ('token, 'a) parser) (f:'a -> ('token, 'b) parser): ('token, 'b) parser= 
    fun s ->
      let g (x, rest) = (f x) rest in
      List.concat_map g (x s)

  let (let*) = (>>=)
      
  (** Parses 'empty' token *)
  let empty v = fun ts -> [(v, ts)]

  (** Fail parser directly fails. (Returns [[]]) *)
  let fail :('token, 'a) parser = fun _ ->  []

  (** Gets next *)
  let get: ('token, 'token) parser = function 
    | [] -> []
    | t::ts -> [(t,ts)]

  let eof = function
    | [] -> [(), []]
    | _ -> []
  
  let exec (p: ('token, 'a) parser ) s = 
    match p s with
    | [(x, [])] -> Result x
    | [] -> Error
    | xs -> Ambiguous (List.map fst xs)

end
open Monad

(** If b is false, fails, otherwise parses unit *)
let check b =
  if b then
    return ()
  else
    fail
open Presyntax

(* DEBUG version *)
let keyword k =
  let* k' = get in
  let* () = check ((Var k) = k') in
  print_string (" + Got keyword " ^ k ^ "\n"); return ()
;;

(*
(* Non-debug version *)
let keyword k =
  let* k' = get in
  check ((Var k) = k')
;;   

*)

(** Symmetric choice of parsers. *)
let (||) (p1:('token, 'a) parser) (p2:('token, 'a) parser) s = 
  List.append (p1 s) (p2 s)

(** Parses zero or more occurances of p. *)

(** Concat of parsers *)
let ( @@ ) p1 p2 =
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
let (<*>) (f_par: ('token, 'a -> 'b) parser) (x_par: ('token, 'a) parser): ('token, 'b) parser = 
  let* f = f_par in
  let* x = x_par in
  return (f x)

(** Map. [f <$> p] Creates a parser that maps f over result of p *)
let (<$>) (f: 'a->'b) (x_parser: ('token, 'a) parser): ('token, 'b) parser = 
  let* x = x_parser in
  return (f x)

(** Parse once with unit parser [p] and yield v*)
let ( >> ) p v =
let* () = p in
return v

(** Parses zero or more occurances of p. *)
let rec iter (p:('token, 'a) parser):('token, 'a list) parser = 
  return []  ||
  (let* x = p in
  let* xs = iter p in
  return (x :: xs))

(** Parses one or more occurances of p. *)
let rec iter1 p =
  let* x = p in
  let* xs = return [] || iter1 p in
  return (x :: xs)

(* Že ok. Rešeno z option -> Tuki nekaj ne štima, mogoče pa vseeno rabimo explicit Error.  *)
(* Vrjetno ok? Zna biti da je option čisto odveč. Ne smemo kar vrnit empty seznama, če so še tokeni !! *)
(** Parses strings matching p between the name parts in (nonempty!) token list/vec *)
let rec between p = function
  | [] -> assert false
  | [k] -> keyword k >> []
  | k :: ks ->
     keyword k <@@
     (let* x = p in
      let* xs = between p ks in
      return (x :: xs))

let numeral =
  let* w = get in
  match w with
    | Predef Int x -> return (Syntax.Int x)
    | _ -> fail

let kw = keyword

let wrapped c1 c2 p =
  let* () = kw c1 in
  let* x = p in
  let* () = kw c2 in
  return x
;;
(* TODO! Why is this string, int list parser automatically? *)

(* let parenth = wrapped "(" ")" *)

(* let html = wrapped "<" ">" ;; *)
 
let array p =
  let* x = wrapped "[" "]" (iter p) in
  return x

let if_then_else_endif p = 
  let ite_parts = ["if"; "then"; "else"; "endif"] in
  let* parts = between p ite_parts in
  match parts with
  | [c; t; e] -> return (Syntax.If (c, t, e))
  | _ -> fail

let data = [
  Var "if";
  Predef (Int 3);
  Var "then";
  Predef (Int 4);
  Var "else";
  Predef (Int 5);
  Var "endif";
]

(* let t = exec (if_then_else_endif numeral @@> eof) data *)

let numlist = List.map (fun x -> Predef (Int (int_of_string x))) (String.split_on_char ' ' "1 2 3 4 5 6")
;;
let t = exec (iter numeral @@ iter numeral @@> eof) 


let language_parser big_p = ((if_then_else_endif big_p) @@> eof)

let main_parse s = 
  let rec
    seq_parser s = language_parser main_parser s
  and
    expr = function
    | Var varname -> (Syntax.Var varname)
    | Seq es  -> (match (exec seq_parser es) with
      | Result x -> x
      | Ambiguous x -> let rec log = function
        | [] -> failwith "Ambiguous parse"
        | x::xs -> print_string (Syntax.string_of_expr x); log xs in
        log x
      | Error -> failwith "Parse error")
    (* TODO! Other predefined *)
    | Predef x -> Syntax.predef_cascade expr x
  and
    main_parser = function
    | [] -> []
    | x::y::rest -> let ex = expr x in [Syntax.Apply(ex,  expr y), rest; ex ,y::rest] (* Try parsing 2 expr? and go forward etc. *)
    | x::rest -> [expr x, rest] (* Try parsing 2 expr and go forward etc. *)

in expr s
;;

let parse_presyntax (env) (presyn_expr) = 
  print_endline ("Presyntax:" ^ (Presyntax.string_of_expr presyn_expr)) ;
  let e = main_parse presyn_expr in 
  print_endline ("Syntax:" ^(Syntax.string_of_expr e)) ;
  e
;;