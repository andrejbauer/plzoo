(** Abstract syntax *)

(** The type of identifiers *)
type name = string

(** Arithmetical operations *)
type arithop = Plus | Minus | Times | Divide | Remainder

(** Comparisons *)
type cmpop = Less | Equal | Unequal

(** Logical operators *)
type boolop = And | Or

(** Expressions *)
type expr =
  | Var of name                      (** variable *)
  | Bool of bool                     (** boolean constant [true] or [false] *)		
  | Int of int                       (** integer constant *)
  | ArithOp of arithop * expr * expr (** arithmetical operation [e1 op e2] *)
  | Not of expr                      (** logical negation [not e] *)
  | CmpOp of cmpop * expr * expr     (** comparison [e1 cmp e2] *)
  | BoolOp of boolop * expr * expr   (** logical operator [e1 op e2] *)
  | If of expr * expr * expr         (** conditional statement [if e1 then e2 else e3] *)
  | Skip                             (** command [skip], does nothing *)
  | Seq of expr * expr               (** sequencing of expressions [e1; e2] *)
  | Let of name * expr * expr        (** local definition [let x = e1 in e2] *)
  | App of expr * expr               (** application [e1 e2] *)
  | Fun of name * expr               (** function [fun x -> e] *)
  | This                             (** the object [this] *)
  | Object of (name * expr) list     (** object with given attributes [{a1=e1, ..., an=en}] *)
  | Copy of expr                     (** (shallow) copy of an object [copy e] *)
  | With of expr * expr              (** object extension [e1 with e2] *)
  | Project of expr * name           (** attribute projection [e.x] *)
  | Assign of expr * name * expr     (** set the value of an attribute [e1.x := e2] *)


(** Expressions evaluate to objects which are represented by the type [ob]. *)
type ob =
  | ObjInt of int                    (** integer *)
  | ObjBool of bool                  (** boolean *)
  | ObjFunc of closure               (** closure (represents a function) *)
  | ObjDict of (name * ob ref) list  (** object [{a1=e1, ..., an=en}] *)
  | ObjWith of ob * ob               (** extended object [ob1 with ob2] *)

(** A closure [(th, (x, env, e))] represents a function [fun x -> e] in
    environment [th, env], where [th] is the value of object [this] and [env]
    is the environment of local definitions accessible by the function. *)
and closure = ob option * (name * env * expr)

(** An environment is a list of pairs [(x,ob)], mapping a variable [x] to
    a value [ob]. *)
and env = (name * ob) list

(** Toplevel commands *)
type toplevel_cmd =
    Expr of expr (** Expressions *)
  | Def of name * expr (** Global definition [let x = e] *)
  | Use of string (** Include file [$use "<filename>"] *)
  | Quit (** Exit toplevel [$quit] *)
