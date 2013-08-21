(** Context management *)

(** A context is represented as an associative list which maps a variable [x] to a pair
   [(t,e)] where [t] is its type and [e] is its value (optional).
*)

(** The entries in the context are declarations of parameters or definitions.
    A parameter declaration carries its type, while a definition carries the type and
    the defining expression. *)
type declaration = 
  | Parameter of Syntax.sort
  | Definition of Syntax.sort * Syntax.term

(** A context consists of a list of names, used for pretty-printing and
    desugaring of variable names to de Bruijn indices, and a list of
    declarations. *)
type context = {
  names : string list ;
  decls : declaration list
}

(** On the zeroth day there was the empty context. *)
let empty_context = {
  names = [] ;
  decls = []
}

(** Drop the most recently added thing from the context. *)
let drop {names = ns; decls = ds} = {names = List.tl ns; decls = List.tl ds}

(** [lookup k ctx] returns the type or definition of [Var k] in context [ctx]. *)
let lookup k {decls=lst} =
  match List.nth lst k with
    | Parameter t -> Parameter (Syntax.shift (k+1) t)
    | Definition (t, e) -> Definition (Syntax.shift (k+1) t, Syntax.shift (k+1) e)
    
(** [lookup_ty k ctx] returns the type of [Var k] in context [ctx]. *)
let lookup_ty k {decls=lst} =
  match List.nth lst k with
    | Parameter t | Definition (t, _) -> Syntax.shift (k+1) t

(** [lookup_definition k ctx] returns the definition of [Var k] in context [ctx]. *)
let lookup_definition k {decls=lst;names=names} = 
  match List.nth lst k with
    | Definition (_, e) -> Some (Syntax.shift (k+1) e)
    | Parameter _ -> None

(** [add_parameter x t ctx] returns [ctx] with the parameter [x] of type [t]. *)
let add_parameter x t ctx =
  { names = x :: ctx.names ;
    decls = Parameter t :: ctx.decls }

(** [add_definition x t e ctx] returns [ctx] with [x] of type [t] defined as [e]. *)
let add_definition x t e ctx =
  { names = x :: ctx.names ;
    decls = Definition (t, e) :: ctx.decls }
