(** Pretty-printing of expressions with the Ocaml [Format] library. *)

(** Print an identifier *)
let ident x ppf =
  Zoo.print ppf "%s" x

(** [lambda e ppf] prints abstraction using formatter [ppf]. *)
let rec lambda xs y e ppf =
  let rec collect ((e',_) as e) =
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
    Zoo.print ~at_level:3 ppf "λ %t .@ %t" (Zoo.print_sequence ident ys) (expr xs e)

(** [expr ctx e ppf] prints expression [e] using formatter [ppf]. *)
and expr ?max_level xs e ppf =
  let rec expr ?max_level xs (e, _) ppf = expr' ?max_level xs e ppf
  and expr' ?max_level xs e ppf =
    let print ?at_level = Zoo.print ?max_level ?at_level ppf in
      if not (Format.over_max_boxes ()) then
        match e with
          | Syntax.Var k -> print "%s" (List.nth xs k)
          | Syntax.Subst (s, e) -> let e = Syntax.subst s e in print "%t" (expr ?max_level xs e)
          | Syntax.Lambda (y, e) -> print ~at_level:3 "%t" (lambda xs y e)
          | Syntax.App (e1, e2) -> print ~at_level:1 "%t@ %t" (expr ~max_level:1 xs e1) (expr ~max_level:0 xs e2)
  in
    expr ?max_level xs e ppf
    
(* let expr' xs e ppf = expr xs (e, Zoo.Nowhere) ppf *)

(** Support for printing of errors, warning and debugging information. *)

let verbosity = ref 2

let message ?(loc=Zoo.Nowhere) msg_type v =
  if v <= !verbosity then
    begin
      Format.eprintf "%s at %t:@\n@[" msg_type (Zoo.print_position loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@]@.") Format.err_formatter
    end
  else
    Format.ifprintf Format.err_formatter

let error (loc, err_type, msg) = message ~loc (err_type) 1 "%s" msg
let warning msg = message "Warning" 2 msg
let debug msg = message "Debug" 3 msg
