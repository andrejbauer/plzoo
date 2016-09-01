(** Pretty-printing of expressions with the Ocaml [Format] library. *)

(** Print an identifier *)
let ident ppf x =
  Format.fprintf ppf "%s" x

(** Print a sequence *)
let sequence ?(sep=" ") printer lst ppf =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
    printer
    ppf
    lst
    
(** [lambda e ppf] prints abstraction using formatter [ppf]. *)
let rec lambda xs y e ppf =
  let rec collect ({Zoo.data=e';_} as e) =
    match e' with
      | Syntax.Subst (s, e) -> let e = Syntax.subst s e in collect e
      | Syntax.Lambda (y, e) -> 
        let ys, k, e = collect e in ((y,k) :: ys), k+1, e
      | Syntax.Var _ | Syntax.App _ -> [], 0, e
  in
  let ys, k, e = collect e in
  let ys = (y, k) :: ys in
  let (ys, _) =
    List.fold_right
      (fun (y,k) (ys, xs) ->
        let y = (if Syntax.occurs k e then Beautify.refresh y xs else "_") in
          (y::ys, y::xs))
      ys ([], xs)
  in
  let xs = (List.rev ys) @ xs
  in
    Zoo.print_parens ~at_level:3 ppf "λ %t .@ %t"
                     (sequence ident ys)
                     (expr xs e)

(** [expr ctx e ppf] prints expression [e] using formatter [ppf]. *)
and expr ?max_level xs e ppf =
  let rec expr ?max_level xs {Zoo.data=e} ppf = expr' ?max_level xs e ppf
  and expr' ?max_level xs e ppf =
    let print ?at_level = Zoo.print_parens ?max_level ?at_level ppf in
      if not (Format.over_max_boxes ()) then
        match e with
          | Syntax.Var k -> print "%s" (List.nth xs k)
          | Syntax.Subst (s, e) -> let e = Syntax.subst s e in print "%t" (expr ?max_level xs e)
          | Syntax.Lambda (y, e) -> print ~at_level:3 "%t" (lambda xs y e)
          | Syntax.App (e1, e2) -> print ~at_level:1 "%t@ %t" (expr ~max_level:1 xs e1) (expr ~max_level:0 xs e2)
  in
    expr ?max_level xs e ppf
