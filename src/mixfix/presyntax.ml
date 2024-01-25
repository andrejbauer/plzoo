(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(** Mixfix expressions *)
type expr =
  | Var of name          (** variable *)
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
  | Fun of name * Syntax.htype * expr (** function [fun x:t -> e] *)
  | Seq of expr list (** unparsed sequence *)
  | Pair of expr * expr  (** pair [(e1, e2)] *)
  | Fst of expr          (** first projection [fst e] *)
  | Snd of expr          (** second projection [snd e] *)
  | Rec of name * Syntax.htype * expr (** recursion [rec x:t is e] *)
  | Nil of Syntax.htype         (** empty list *)
  | Cons of expr * expr  (** cons list [e1 :: e2] *)
  | Match of expr * Syntax.htype * expr * name * name * expr
      (** list decomposition [match e with [t] -> e1 | x::y -> e2] *)

let split_to_functions ((a:(string * Syntax.htype) list), (e:expr)) =
      List.fold_right (fun (x, ty) e -> Fun (x, ty, e)) a e


(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
  | MixDef of string * name * int * expr 
  | Quit               (** exit toplevel [$quit] *)

(* NOTE: No need to bother with printing Seq, because it will be always called from Syntax *)