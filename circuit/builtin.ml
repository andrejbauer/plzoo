(* Builtin functions. *)

open Syntax

let plus = function
  | [Eval.Int k; Eval.Int m] -> Eval.Int (k + m)
  | _ -> Eval.runtime_error "Two integers expected"

let minus = function
  | [Eval.Int k; Eval.Int m] -> Eval.Int (k - m)
  | _ -> Eval.runtime_error "Two integers expected"

let times = function
  | [Eval.Int k; Eval.Int m] -> Eval.Int (k * m)
  | _ -> Eval.runtime_error "Two integers expected"

let builtin =
  [ ("plus",  ([TyInt; TyInt], TyInt), plus) ;
    ("minus", ([TyInt; TyInt], TyInt), minus) ;
    ("times", ([TyInt; TyInt], TyInt), times) ]

let env = List.fold_right (fun (f,_,v) env -> Eval.extend_func env f v) builtin Eval.empty_env

let ctx = List.fold_right (fun (f,(ts,t),_) ctx -> Type_check.extend_func ctx f ts t) builtin Type_check.empty_ctx

