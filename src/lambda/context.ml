(** Context management *)

(** A context is represented as an associative list which maps a variable [x] to
    an optional value. *)

(** The entries in the context are declarations of parameters or definitions. *)
type declaration =  Syntax.term option

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

(** [lookup_definition k ctx] returns the definition of [Var k] in context [ctx]. *)
let lookup_definition k {decls=lst} = 
  match List.nth lst k with
    | Some e -> Some (Syntax.shift (k+1) e)
    | None -> None

(** [add_parameter x ctx] returns [ctx] with the parameter [x]. *)
let add_parameter x ctx =
  { names = x :: ctx.names ;
    decls = None :: ctx.decls }

(** [add_definition x e ctx] returns [ctx] with [x] defined as [e]. *)
let add_definition x e ctx =
  { names = x :: ctx.names ;
    decls = Some e :: ctx.decls }
