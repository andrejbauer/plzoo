type name = string

type expr =
    Id of name
  | Lam of name * expr
  | App of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr

type toplevel =
    Definition of name * expr
  | Expr of expr

let lamda = "λ"
(* let lamda = "^" *)

let string_of_expr e =
  let rec str lvl e =
    let lvl', s = match e with
	Id x -> 0, x
      | App (e1, e2) -> 2, (str 2 e1) ^ " " ^ (str 1 e2)
      | Lam _ as e ->
	  begin
	    let rec collect = function
		Lam (x, e) -> let lst, e' = collect e in x::lst, e'
	      | (Id _ | App _ | Pair _ | Fst _ | Snd _) as e -> [], e
	    in
	    let lst, e' = collect e in
	      3, lamda ^ (String.concat " " lst) ^ "." ^ (str 3 e')
	  end
      | Pair (e1, e2) -> 0, "(" ^ (str 2 e1) ^ ", " ^ (str 2 e2) ^ ")"
      | Fst e -> 2, "fst " ^ (str 1 e)
      | Snd e -> 2, "snd " ^ (str 1 e)
    in
      if lvl' > lvl then "(" ^ s ^ ")" else s
  in
    str 99 e

let rec fv = function
    Id x -> [x]
  | Lam (x, e) -> List.filter ((<>) x) (fv e)
  | App (e1, e2) -> (fv e1) @ (fv e2)
  | Pair (e1, e2) -> (fv e1) @ (fv e2)
  | Fst e -> fv e
  | Snd e -> fv e

let next x = x ^ "'"

let rec fresh x bad =
  if not (List.mem x bad) then
    x
  else 
    fresh (next x) bad

let shadow x env = List.filter (fun (y,_) -> x <> y) env

let rec subst s = function
    Id x as e ->
      (try List.assoc x s with Not_found -> e)
  | App (e1, e2) -> App (subst s e1, subst s e2)
  | Pair (e1, e2) -> Pair (subst s e1, subst s e2)
  | Fst e -> Fst (subst s e)
  | Snd e -> Snd (subst s e)
  | Lam (x, e) ->
      let x' = fresh x (List.concat (List.map (fun (_, e) -> fv e) s)) in
	Lam (x', subst ((x, Id x')::s) e)
