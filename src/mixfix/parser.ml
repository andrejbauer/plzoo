

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

  let check_success (env:Environment.t) lst =
    let lst =  List.filter (fun x ->
      try (
        let _ = Type_check.type_of (env.context) x in true) with 
      _ -> false
    ) lst
    in match lst with
    | [] -> Zoo.error "could not parse"
    | [r] -> r
    | _ :: _ :: _ as e -> print_endline (String.concat "\n"(List.map Syntax.string_of_expr e) ); Zoo.error "ambiguous parse"

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

  (* let type_p ty context p =
    let* x = p in
    (try
      Type_check.check context ty x; return x
    with
    | _ -> fail) *)
  (* TODO To ni kul *)
  
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

    (* TODO! Tu pride v poštev <*> *)
  let (<**>) xl_par =
    let* xs = xl_par in
    return (fun x -> xs @ [x])

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

  let rec between p = function
    | [] -> assert false
  | [k] -> kw k >> []
  | k :: ks ->
      kw k <@@
     (let* x = p in
     let* xs = between p ks in
     return (x :: xs))

     (** Between that maps to presyntax *) 
  let betweenp p k = 
    let k = List.map (fun x -> Presyntax.Var x) k in
    between p k

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

  let rec expr env e : Syntax.expr list =
    let open ListMonad in
    match e with

  | Presyntax.Var x ->
      if List.mem_assoc x Environment.(env.context) then
        return @@ Syntax.Var x
     else
       (print_endline (" + Undefined variable " ^ x); fail)

  | Presyntax.Seq es  ->
      unwrap @@ (get_parser env) es 

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
  let g = env.operators in

  let rec precs (ps: Precedence.graph) = 
    match ps with
    | [] -> app_parser env
    | p::ps -> 
        let sucs = precs ps in 
        prec sucs (snd p) ||| sucs

      and prec sucs = function
        | [] -> fail
        | o::os -> op sucs o ||| prec sucs os

      and op up operator = 
        let same_or_up = up in
        let op_name = Syntax.Var (Syntax.op_name operator) in
        let appr res b = List.fold_right (fun a b -> (Syntax.make_app op_name (a @ [b]))) res b in
        let appl a res = List.fold_left (fun a b -> (Syntax.make_app op_name (a::b))) a res in
      match operator with

    | {fx=Closed; tokens} ->
        let* res = betweenp same_or_up tokens in
        return @@ Syntax.make_app op_name res

    | {fx=Postfix; tokens} ->
        let* head = up in
        let* res = iter1 @@ betweenp same_or_up tokens in
        return @@ appl head res

    | {fx=Prefix; tokens} ->
        let* res = iter1 @@ betweenp same_or_up tokens in
        let* b = up in
        return @@ appr res b 

    | {fx=Infix NonAssoc; tokens} ->
      let* a = up in
      let* mid = betweenp same_or_up tokens in
      let* b = up in 
      return @@ Syntax.make_app op_name (a::mid @ [b])  

    | {fx=Infix LeftAssoc; tokens} ->
      let* a = up in
      let* mid = iter1 @@ betweenp same_or_up tokens in
      let* b = up in 
      (match mid with
        | [] -> fail
        | head::tail -> let head = a::head in return @@ appr ( head::tail) b
      )

    | {fx=Infix RightAssoc; tokens} ->
      let* a = up in
      let* mid = iter1 @@ betweenp same_or_up tokens in
      let* b = up in 
      let rec f = function
        | [] -> []
        | [last] -> [last @ [b]]
        | head::tail -> (head::(f tail))
      in
        return @@ appl a (f mid)

      in precs g

      (** Čas za <*> ? *)
let s (ts:string list)= ((fun a b -> [a; b]) <$> get) ts
let k1 = kw "hello" >> "s";;

let p = (s <*> k1);;

let t = p ["hello"; "world"];;