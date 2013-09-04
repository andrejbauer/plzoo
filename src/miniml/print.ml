let ty t ppf =
  let rec ty ?max_level t ppf =
    if not (Format.over_max_boxes ()) then
      match t with
        | Syntax.TInt -> Zoo.print ppf "int"
        | Syntax.TBool -> Zoo.print ppf "bool"
        | Syntax.TArrow (t1, t2) -> 
          Zoo.print ppf ~at_level:1 "%t ->@ %t" (ty ~max_level:1 t1) (ty ~max_level:0 t2)
  in
    ty ~max_level:9999 t ppf

let mvalue m ppf =
  match m with
    | Machine.MInt k -> Zoo.print ppf "%d" k
    | Machine.MBool b -> Zoo.print ppf "%b" b
    | Machine.MClosure _ -> Zoo.print ppf "<fun>"

  
