(** Type checking. *)

open Syntax

type context = {
  funcs : (name * (ty list * ty)) list ;
  vars : (name * ty) list
}

let empty_ctx = { funcs = []; vars = [] }

let extend_var ctx x t = { funcs = ctx.funcs ; vars = (x,t) :: ctx.vars }

let extend_func ctx f ts t = { funcs = (f, (ts, t)) :: ctx.funcs ; vars = ctx.vars }

(** Exception indicating a type error. *)
exception Type_error of string

(** [ty_error msg] reports a type error. *)
let type_error msg = raise (Type_error msg)

let check_linear x e =
  if count x e > 1 then type_error ("variable " ^ x ^ " is used more than once")

(** [check ctx ty e] verifies that expression [e] has type [ty] in
    context [ctx]. If it does, it returns unit, otherwise it raises the
    [Type_error] exception. *)
let rec check ctx ty e =
  let ty' = type_of ctx e in
    if ty' <> ty then
      type_error
	(string_of_expr e ^ " has type " ^ string_of_ty ty' ^
	   " but is used as if it has type " ^ string_of_ty ty)

(** [type_of ctx e] computes the type of expression [e] in context
    [ctx]. If [e] does not have a type it raises the [Type_error]
    exception. *)
and type_of ctx = function
  | Var x ->
      (try List.assoc x ctx.vars with
	   Not_found -> type_error ("unknown variable " ^ x))
  | Int _ -> TyInt
  | Bool _ -> TyBool
  | String _ -> TyString
  | Cond (e1, e2, e3) ->
    check ctx TyBool e1 ;
    let t2 = type_of ctx e2 in
    check ctx t2 e3 ;
    t2

  | Apply (f, lst) ->
      let (ts, t) = (try List.assoc f ctx.funcs with Not_found -> type_error ("unknown function " ^ f)) in
      if List.length ts <> List.length lst then
        type_error ("function " ^ f ^ " expects " ^ string_of_int (List.length ts) ^
                       " but is applied to " ^  string_of_int (List.length lst) ^ " arguments")
      else begin
        List.iter (fun (t,e) -> check ctx t e) (List.combine ts lst) ;
        t
      end      
  | Let (x, e1, e2) ->
    let t1 = type_of ctx e1 in
    type_of (extend_var ctx x t1) e2
  | Copy (e1, x, y, e2) ->
    if x = y
    then type_error "variable names in copy must be distinct"
    else
      let t1 = type_of ctx e1 in
      let ctx = extend_var ctx x t1 in
      let ctx = extend_var ctx y t1 in
      type_of ctx e2

let type_of_def ctx lst e =
  let rec check_all_different = function
    | [] -> ()
    | x::lst ->
      if List.mem x lst
      then type_error ("argument " ^ x ^ " appears twice")
      else check_all_different lst
  in
  check_all_different (List.map snd lst) ;
  let ctx = List.fold_right (fun (t,x) ctx -> extend_var ctx x t) lst ctx in
  let t = type_of ctx e in
    List.iter (fun (_,x) -> check_linear x e) lst ;
    (List.map fst lst, t)
  
