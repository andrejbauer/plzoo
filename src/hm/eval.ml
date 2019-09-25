open Syntax

(** Global Environment *)

type env = value NameMap.t
let initial_env = NameMap.empty
let add = NameMap.add
let find x env =
  if NameMap.mem x env then
    NameMap.find x env
  else
    Zoo.error "Unbound variable %a" Printer.name x

(** Substitutions *)

let rec subst_value x v = function
  | Constant c -> Constant c
  | Lambda (y,e) when not @@ Name.equal x y ->
    (Lambda (y, subst x v e))
  | Lambda (_, _) 
  | Y 
  | Ref _
    as v -> v

(** e[x -> v] *)
and subst x v e = match e with
  | Var y when Name.equal x y -> V v
  | Var n -> Var n
  | V v' -> V (subst_value x v v')
  | App (f,e) -> App (subst x v f, List.map (subst x v) e)
  | Let (y,e1,e2) when not @@ Name.equal x y ->
    Let (y, subst x v e1, subst x v e2)
  | Let (y,e1,e2) ->
    Let (y, subst x v e1, e2)

let subst_env = NameMap.fold subst

(** Evaluation *)

let const x = V (Constant x)
let delta c v = match c,v with
  | Int _, [] -> None
    
  | Plus, [ Constant (Int i) ; Constant (Int j) ] ->
    Some (V (Constant (Int (i + j))))
  | Plus, [ Constant (Int i) ] ->
    let n = Name.create ~name:"i" () in
    Some (V (Lambda (n, App (const Plus, [const @@ Int i; Var n]))))
      
  | NewRef, [ v ] -> Some (V (Ref (ref v)))
  | Get, [ Ref r ] -> Some (V !r)
  | Set, [ Ref r ] ->
    let n = Name.create ~name:"r" () in
    Some (V (Lambda (n, App (const Set, [V (Ref r); Var n]))))
  | Set, [ Ref r ; v ] -> r := v ; Some (V v)
    
  | _ -> None

exception Not_reducible : expr -> exn

let log_eval i = Format.printf "%s< %a@." (String.make i ' ') Printer.expr
let log_val i = Format.printf "%s> %a@." (String.make i ' ') Printer.value

let reduction_failure e = 
  Zoo.error ~kind:"Execution error"
    "The following expression can not be reduced:@.%a" Printer.expr e

let rec eval i e = match e with
  | V v -> v
  | Var _ -> reduction_failure e
  | Let (x,e1,e2) ->
    (* log_eval i e ; *)
    let v = eval (i+1) e1 in
    let v' = eval (i+1) @@ subst x v e2 in
    (* log_val i v' ; *)
    v'
  | App(f,l) ->
    (* log_eval i e ; *)
    let vf = eval (i+1) f in
    let ve = List.map (eval @@ i+1) l in
    let v = eval_app (i+1) e vf ve in
    (* log_val i v ; *)
    v   

and eval_app i eorig f l = match f, l with
  | _, [] -> f
  | Ref _, _ -> reduction_failure eorig
  | Y, ve::t ->
    let n = Name.create ~name:"Y" () in
    eval_app i eorig ve (Lambda(n, App(V Y, [V ve; Var n])) :: t)
  | Lambda(x, body), (v :: t) ->
    eval_app i eorig (eval (i+1) @@ subst x v body) t
  | Constant c, l ->
    begin match delta c l with
      | Some x -> eval (i+1) x
      | None -> reduction_failure eorig
    end

let execute env p = eval 0 @@ subst_env env p
