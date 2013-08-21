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
	      3, lambda ^ (String.concat " " lst) ^ "." ^ (str 3 e')
	  end
      | Pair (e1, e2) -> 0, "(" ^ (str 2 e1) ^ ", " ^ (str 2 e2) ^ ")"
      | Fst e -> 2, "fst " ^ (str 1 e)
      | Snd e -> 2, "snd " ^ (str 1 e)
    in
      if lvl' > lvl then "(" ^ s ^ ")" else s
  in
    str 99 e

