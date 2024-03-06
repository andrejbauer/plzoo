

(* Parser combinatorji *)

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

  val returnMany : 'a list -> ('token, 'a) t

  val ( let* ) : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t

  val ( >>= )  : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t

  val empty : 'a -> ('token, 'a) t

  val fail : ('token, 'a) t

  val get : ('token, 'token) t

  val eof : ('token, unit) t

  val ( ||| ) : ('token, 'a) t -> ('token, 'a) t -> ('token, 'a) t
end = struct

  let return x = fun s -> [(x, s)]

  let returnMany xs = fun s -> List.map (fun x -> (x, s)) xs

  let (>>=) (x: ('token, 'a) t) (f:'a -> ('token, 'b) t): ('token, 'b) t=
    fun s ->
    let g (x, rest) = (f x) rest in
    List.concat_map g (x s)

  let ( let* ) = ( >>= )

  (** Parses 'empty' token *)
  let empty v = fun ts -> [(v, ts)]

  (** Fail parser directly fails. (Returns [[]]) *)
  let fail :('token, 'a) t = fun _ ->  []

  (** Gets next *)
  let get: ('token, 'token) t = function
    | [] -> []
    | t::ts -> [(t,ts)]

  let ( ||| ) c1 c2 s = (c1 s) @ (c2 s)

  let eof = function
    | [] -> [(), []]
    | _ -> []
end

let check_success (env:Environment.t) lst =
  match lst with
  | [] -> Zoo.error "could not parse"
  | [r] -> r
  | _ :: _ :: _ as e ->
     print_endline (String.concat "\n" (List.map Syntax.string_of_expr e) ) ;
     Zoo.error "ambiguous parse"

open Monad

type (_,_) parser =
  | Fail : ('a, 'b) parser
  | Empty : ('a, unit) parser
  | Kw : 'a -> ('a, unit) parser
  | Or : ('a, 'b) parser * ('a, 'b) parser -> ('a, 'b) parser
  | Cons : ('a, 'b) parser * ('a, 'c) parser -> ('a, 'b * 'c) parser
  | ConsAfter : ('a, 'b) parser * ('a, unit) parser -> ('a, 'b) parser
  | ConsBefore : ('a, unit) parser * ('a, 'b) parser -> ('a, 'b) parser
  | App : ('a, 'b -> 'c) parser * ('a, 'b) parser -> ('a, 'c) parser
  | Map : ('b -> 'c) * ('a, 'b) parser -> ('a, 'c) parser
  | List : ('a, 'b) parser * ('a, 'b list) parser -> ('a, 'b list) parser
  | Check : ('a, unit) parser * 'b -> ('a, 'b) parser
  | Between : ('a, 'b) parser * 'a list -> ('a, 'b list) parser
  | Betweenp : (Presyntax.expr, 'b) parser * string list -> (Presyntax.expr, 'b list) parser
  | Lazy : ('a, 'b) parser lazy_t -> ('a, 'b) parser
  | Get : ('a, 'a) parser
  | Fold : ('a list -> 'b list) -> ('a, 'b) parser

(** Smart parser constructors *)

let recursively build =
  let rec self = lazy (build self) in
  Lazy.force self

let iter p =
  recursively (fun self -> Or (Check (Empty, []), Map ((fun (a, b) -> a :: b), Cons (p, Lazy self))))

let iter1 p =
  Map ((fun (a,b) -> a :: b), Cons (p, iter p))

(** Check. [p <* v] Parses p and then checks if v is true. *)

(** Parse once with unit parser [p] and yield v*)
let ( >> ) p v =
  Check (p, v)

let kw k = Kw k

let rec runParser : type a b . (a, b) parser -> (a, b) t = function

  | Fail -> fail

  | Empty -> return ()

  | Kw k ->
     let* k' = get in
     if (k = k') then
       return ()
     else
       fail

  | Or (p, q) -> runParser p ||| runParser q

  | Cons (p, q) ->
     let* x = runParser p in
     let* y = runParser q in
     return (x, y)

  | ConsAfter (p, q) ->
     let* x = runParser p in
     let* () = runParser q in
     return x

  | ConsBefore (p, q) ->
     let* () = runParser p in
     let* x = runParser q in
     return x

  | App (f, x) ->
     let* f = runParser f in
     let* x = runParser x in
     return (f x)

  | Map (f, x) ->
     let* x = runParser x in
     return (f x)

  | Check (p, v) ->
     let* () = runParser p in
     return v

  | List (xp, xsp) ->
     let* x = runParser xp in
     let* xs = runParser xsp in
     return (x :: xs)

  | Between (p, []) -> assert false
  | Between (p, [k]) ->
     runParser (Kw k >> [])
  | Between (p, k :: ks) ->
     runParser @@
       ConsBefore (Kw k, List(p, Between (p, ks)))

  | Betweenp (p, ts) ->
     let ts = List.map (fun x -> Presyntax.Var x) ts in
     runParser @@ Between (p, ts)

  | Fold f ->
     let* p = runParser @@ iter Get in
     returnMany @@ f p

  (* | AppParser (op, args) -> *)
  (*   let* args = runParser args in *)
  (*   return @@ Syntax.make_app (Syntax.Var (Syntax.op_name op)) args *)

  (* | AppParserLeft (op, stronger, between) -> *)
  (*     let* head = runParser @@ stronger in *)
  (*     let* tails = runParser @@ between in *)
  (*     let opname = Syntax.Var (Syntax.op_name op) in *)
  (*     return @@ List.fold_left (fun a b -> Syntax.make_app opname (a::b)) head tails *)

  (* | AppParserRight (op, between, stronger) -> *)
  (*   let* heads = runParser @@ between in *)
  (*   let* last = runParser @@ stronger in *)
  (*   let opname = Syntax.Var (Syntax.op_name op) in *)
  (*   return @@ List.fold_right (fun a b -> Syntax.make_app opname (a @ [b])) heads last *)

  | Get -> get

  | Lazy (lazy p) ->
     runParser p

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

(* let type_p ty context p =
   let* x = p in
   (try
   Type_check.check context ty x; return x
   with
   | _ -> fail) *)
(* TODO To ni kul *)

(** Concat of parsers *)
let ( @@@ ) p1 p2 =
  Cons (p1, p2)

(** Concat ignore right unit *)
let ( @@> ) p1 p2 =
  ConsAfter (p1, p2)

(** Concat ignore left unit *)
let ( <@@ ) p1 p2 =
  ConsBefore (p1, p2)

(** "Applicative functor apllication" *)
let (<*>) f_par x_par  =
  App (f_par, x_par)

(** Map. [f <$> p] Creates a parser that maps f over result of p *)
let (<$>) f x_parser =
  Map (f, x_parser)


(** Parse once with unit parser [p] and yield v*)

(* example, does not belong here. *)
(* let factorial = *)
(*   fix (fun self -> fun n -> if n = 0 then 1 else n * self () (n - 1)) *)

let between p tokens = Between (p, tokens)

(** Between that maps to presyntax *)
let betweenp p k =
  let k = List.map (fun x -> Presyntax.Var x) k in
  Between (p, k)

let numeral =
  let* w = get in
  match w with
  | Presyntax.Int x -> return (Syntax.Int x)
  | _ -> fail

let if_then_else_endif p =
  let ite_parts = Presyntax.([Var "if"; Var "then"; Var "else"; Var "endif"]) in
  let* parts = runParser @@ Between (p, ite_parts) in
  match parts with
  | [c; t; e] -> return (Syntax.If (c, t, e))
  | _ -> fail

let rec expr env e : Syntax.expr list =
  let open ListMonad in
  match e with
  | Presyntax.Var x ->
     if List.mem_assoc x Environment.(env.context) then
       return @@ Syntax.Var x
     else
       fail

  | Presyntax.Seq es  ->
     unwrap @@ runParser (get_parser env) es

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

  | Presyntax.Fun (x, ht, e) -> (* IMPORTANT! Add x to env*) (* MENTION *)
     let env = { env with context = (x, ht) :: env.context} in
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

and app_parser env: (Presyntax.expr, Syntax.expr) parser =
  let open ListMonad in
  let f = function
    | [] -> fail
    | h :: args ->
       let rec map_expr = function
         | [] -> return []
         | arg :: args ->
            let* arg = expr env arg in
            let* args = map_expr args in
            return (arg :: args)
       in
       let* h = expr env h in
       let* args = map_expr args in
       return @@ Syntax.make_app h args
  in
  Fold f

and get_parser env : (Presyntax.expr, Syntax.expr) parser =
  let g = env.operators in

  let rec graph_parser (g: Precedence.graph): (Presyntax.expr, Syntax.expr) parser =
    recursively (fun self ->
        let rec precedence_parser stronger operators =
          let operator_parser stronger_parser op =
            let ophead = Syntax.Var (Syntax.op_name op) in
            match op with

            | Syntax.{fx=Closed; tokens} ->
               Map (Syntax.make_app ophead, Betweenp (Lazy self, tokens))

            | {fx=Postfix; tokens} ->
               Map ((fun (head, tails) -> List.fold_left (fun a b -> Syntax.make_app ophead (a :: b)) head tails),
                    Cons (stronger_parser, iter1 (Betweenp (Lazy self, tokens))))

            | {fx=Prefix; tokens} ->
               Map ((fun (tails, head) -> List.fold_right (fun b a -> Syntax.make_app ophead (b @ [a])) tails head),
                    Cons (iter1 (Betweenp (Lazy self, tokens)), stronger_parser))

            | {fx=Infix NonAssoc; tokens} ->
               (* AppParser( *)
               (*     operator, *)
               (*     Map ( *)
               (*         (fun (a, (mid, b)) -> (a::mid @ [b])), *)
               (*         Cons( *)
               (*             stronger_parser, *)
               (*             Cons( *)
               (*                 Betweenp (Lazy self, tokens), *)
               (*                 stronger_parser *)
               (*               ) *)
               (*           ) *)
               (*       ) *)
               (*   ) *)
               failwith "Infix NonAssoc"

            | {fx=Infix LeftAssoc; tokens} ->
               (* AppParserRight ( *)
               (*     operator, *)
               (*     Map( *)
               (*         (fun (a,b) -> match b with *)
               (*                       | [] -> [] *)
               (*                       | head::tail -> *)
               (*                          let head = a::head in *)
               (*                          head::tail), *)
               (*         Cons( *)
               (*             stronger_parser, *)
               (*             Iter1 (Betweenp (Lazy self, tokens)) *)
               (*           ) *)
               (*       ), *)
               (*     stronger_parser *)
               (*   ) *)
               failwith "Infix LeftAssoc"

            | {fx=Infix RightAssoc; tokens} ->
               (* AppParserLeft ( *)
               (*     operator, *)
               (*     stronger_parser, *)
               (*     Map( *)
               (*         (fun (mid,b) -> *)
               (*           let rec f = function *)
               (*             | [] -> [] *)
               (*             | [last] -> [last @ [b]] *)
               (*             | head::tail -> (head::(f tail)) *)
               (*           in f mid), *)
               (*         Cons( *)
               (*             Iter1 (Betweenp (Lazy self, tokens)), *)
               (*             stronger_parser *)
               (*           ) *)
               (*       ) *)
               (*   ) *)
               failwith "Infix RightAssoc"
          in

          match operators with
          | [] -> Fail
          | o::os -> Or (operator_parser stronger o, precedence_parser stronger os)
        in
        match g with
        | [] -> app_parser env
        | p::ps ->
           let sucs = graph_parser ps in
           Or (precedence_parser sucs (snd p), sucs)
      )
  in graph_parser g
