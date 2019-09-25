module Name = struct
  type t = {name : string ; id : int}
  let compare n1 n2 = compare n1.id n2.id
  let equal n1 n2 = n1.id = n2.id
  let dummy name = { name ; id = -1 }
  let create =
    let r = ref 0 in
    fun ?(name="") () ->
      let id = !r in incr r ;
      { name ; id }
end
module NameMap = Map.Make(Name)

type constant =
  | Int of int
  | Plus
  | NewRef
  | Get
  | Set

type value =
  | Constant of constant
  | Lambda of Name.t * expr
  | Ref of value ref
  | Y

and expr =
  | V of value
  | Var of Name.t
  | App of expr * expr list
  | Let of Name.t * expr * expr
type command =
  | Def of Name.t * expr

let is_value = function
  | V _ -> true
  | _ -> false


module Rename = struct
  module SMap = Map.Make(String)

  type env = Name.t SMap.t
  
  let find x env =
    if SMap.mem x env then
      SMap.find x env
    else
      Zoo.error "Unbound variable %s" x

  let add n k env = SMap.add n k env

  let rec value env = function
    | Lambda ({name}, e) ->
      let new_name = Name.create ~name () in
      let env = add name new_name env in
      let e = expr env e in
      Lambda (new_name, e)
    | Constant _ | Y | Ref _ as e-> e

  and expr env = function
    | V v -> V (value env v)
    | Var { name } -> Var (find name env)
    | App (f, l) -> App (expr env f, List.map (expr env) l)
    | Let ({name}, e1, e2) ->
      let e1 = expr env e1 in
      let new_name = Name.create ~name () in
      let env = add name new_name env in
      let e2 = expr env e2 in
      Let (new_name, e1, e2)

  let command env = function
    | Def ({name},e) -> 
      let e = expr env e in
      let new_name = Name.create ~name () in
      Def (new_name, e)

end
