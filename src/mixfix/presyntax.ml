(** Abstract syntax *)


(** The type of variable names. *)
type name = string

(** Mixfix types. *)
type htype =
  | TInt (** integer [int] *)
  | TBool (** booleans [bool] *)
  | TTimes of htype * htype  (** Product [s * t] *)
  | TArrow of htype * htype  (** Function type *)
  | TList of htype (** Lists [t list] *)

(** Mixfix expressions *)
type expr =
  | Var of name          (** variable *)
  | Predef of predef
  | Seq of expr list (** unparsed sequence *)
and predef =
  | Int of int           (** integer constant *)
  | Bool of bool         (** boolean constant *)
  | Times of expr * expr (** product [e1 * e2] *)
  | Divide of expr * expr(** quotient [e1 / e2] *)
  | Mod of expr * expr   (** remainder [e1 % e2] *)
  | Plus of expr * expr  (** sum [e1 + e2] *)
  | Minus of expr * expr (** difference [e1 - e2] *)
  | Equal of expr * expr (** integer equality [e1 = e2] *)
  | Less of expr * expr  (** integer comparison [e1 < e2] *)
  | If of expr * expr * expr (** conditional [if e1 then e2 else e3] *)
  | Fun of name * htype * expr (** function [fun x:t -> e] *)
  | Pair of expr * expr  (** pair [(e1, e2)] *)
  | Fst of expr          (** first projection [fst e] *)
  | Snd of expr          (** second projection [snd e] *)
  | Rec of name * htype * expr (** recursion [rec x:t is e] *)
  | Nil of htype         (** empty list *)
  | Cons of expr * expr  (** cons list [e1 :: e2] *)
  | Match of expr * htype * expr * name * name * expr
      (** list decomposition [match e with [t] -> e1 | x::y -> e2] *)


type associativity =
  | LeftAssoc | RightAssoc | NonAssoc

(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
  | Mixfix of associativity * int * string 
  | Quit               (** exit toplevel [$quit] *)


(** Conversion from a type to a string *)
let string_of_type ty =
  let rec to_str n ty =
    let (m, str) =
      match ty with
	  TInt -> (4, "int")
	| TBool -> (4, "bool")
	| TList ty -> (3, to_str 3 ty ^ " list")
	| TTimes (ty1, ty2) -> (2, (to_str 2 ty1) ^ " * " ^ (to_str 2 ty2))
	| TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty


(** Conversion from an expression to a string *)
let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
        | Var v -> (100, v)
      	| Seq exprs ->       (9, "[[" ^ String.concat ";" ( List.map (to_str 8) exprs) ^ "]]")
        | Predef x -> 
          match x with
	| Int n ->           (10, string_of_int n)
	| Bool b ->          (10, string_of_bool b)
	| Pair (e1, e2) ->   (10, "(" ^ (to_str 0 e1) ^ ", " ^ (to_str 0 e2) ^ ")")
	| Nil ty ->          (10, "[" ^ (string_of_type ty) ^ "]")
	| Fst e ->           (9, "fst " ^ (to_str 9 e))
	| Snd e ->           (9, "snd " ^ (to_str 9 e))
	| Times (e1, e2) ->  (9, "$times(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Divide (e1, e2) -> (9, "$divide(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Mod (e1, e2) ->    (9, "$mod(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Plus (e1, e2) ->   (9, "$plus(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Minus (e1, e2) ->  (9, "$minus(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Cons (e1, e2) ->   (9, "$cons(" ^(to_str 8 e1) ^ "," ^(to_str 8 e2) ^ ")")
	| Equal (e1, e2) ->  (9, "$equal(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| Less (e1, e2) ->   (9, "$less(" ^(to_str 8 e1) ^ "," ^ (to_str 8 e2) ^ ")")
	| If (e1, e2, e3) -> (9, "$cond(" ^ (to_str 8 e1) ^ "," ^
				(to_str 8 e2) ^ "," ^ (to_str 8 e3) ^ ")")
	| Match (e1, ty, e2, x, y, e3) ->
	    (3, "match " ^ (to_str 3 e1) ^ " with " ^
	       "[" ^ (string_of_type ty) ^ "] -> " ^ (to_str 3 e2) ^ " | " ^
	       x ^ "::" ^ y ^ " -> " ^ (to_str 3 e3))
	| Fun (x, ty, e) -> 
	    (2, "fun " ^ x ^ " : " ^ (string_of_type ty) ^ " -> " ^ (to_str 0 e))
	| Rec (x, ty, e) -> 
	    (1, "rec " ^ x ^ " : " ^ (string_of_type ty) ^ " is " ^ (to_str 0 e))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e