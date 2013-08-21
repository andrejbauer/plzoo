(** Pretty-printing of expressions with the Ocaml [Format] library. *)

(** Print an expression, possibly placing parentheses around it. We always
    print things at a given "level" [at_level]. If the level exceeds the
    maximum allowed level [max_level] then the expression should be parenthesized.

    Let us consider an example. When printing nested applications, we should print [App
    (App (e1, e2), e3)] as ["e1 e2 e3"] and [App(e1, App(e2, e3))] as ["e1 (e2 e3)"]. So
    if we assign level 1 to applications, then during printing of [App (e1, e2)] we should
    print [e1] at [max_level] 1 and [e2] at [max_level] 0.
*)
let print ?(max_level=9999) ?(at_level=0) ppf =
  if max_level < at_level then
    begin
      Format.fprintf ppf "(@[" ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@])") ppf
    end
  else
    begin
      Format.fprintf ppf "@[" ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@]") ppf
    end

(** Print the given source code position. *)
let position loc ppf =
  match loc with
  | Common.Nowhere ->
      Format.fprintf ppf "unknown position"
  | Common.Position (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in

      if String.length filename != 0 then
        Format.fprintf ppf "file %S, line %d, charaters %d-%d" filename begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1) begin_char end_char

(** Print a sequence of things with the given (optional) separator. *)
let sequence ?(sep="") f lst ppf =
  let rec seq = function
    | [] -> print ppf ""
    | [x] -> print ppf "%t" (f x)
    | x :: xs -> print ppf "%t%s@ " (f x) sep ; seq xs
  in
    seq lst

let rec to_int = function
  | Syntax.Zero, _ -> Some 0
  | Syntax.Succ e, _ -> (match to_int e with Some n -> Some (n+1) | None -> None)
  | Syntax.Subst (s, e), _ -> to_int (Syntax.subst s e)
  | _ -> None

(** [pi xs a ppf] prints abstraction [a] as dependent product using formatter [ppf]. *)
let rec pi ?max_level xs x t1 t2 ppf =
  if Syntax.occurs 0 t2
  then
    let x = Beautify.refresh x xs in
      print ~at_level:3 ppf "forall %s :@ %t,@ %t" x (expr ~max_level:2 xs t1) (expr (x :: xs) t2)
  else
    print ~at_level:3 ppf "%t ->@ %t" (expr ~max_level:2 xs t1) (expr ("_" :: xs) t2)

(** [lambda xs x t e ppf] prints function [fun x => e] using formatter [ppf]. *)
and lambda xs x t e ppf =
  let x =
    if Syntax.occurs 0 e
    then Beautify.refresh x xs 
    else "_"
  in
    match t with
      | None -> print ~at_level:3 ppf "fun %s =>@ %t" x (expr (x :: xs) e)
      | Some t -> print ~at_level:3 ppf "fun %s : %t =>@ %t" x (expr ~max_level:2 xs t) (expr (x :: xs) e)

(** [expr ctx e ppf] prints expression [e] using formatter [ppf]. *)
and expr ?max_level xs e ppf =
  let rec expr ?max_level xs (e, _) ppf = expr' ?max_level xs e ppf
  and expr' ?max_level xs e ppf =
    let print ?at_level = print ?max_level ?at_level ppf in
      if not (Format.over_max_boxes ()) then
        match e with
          | Syntax.Var k -> print "%s" (List.nth xs k)
          | Syntax.Subst (s, e) -> let e = Syntax.subst s e in print "%t" (expr ?max_level xs e)
          | Syntax.Id (e1, e2, t) -> print ~at_level:3 "%t = %t @@ %t"
                                       (expr ~max_level:2 xs e1) (expr ~max_level:2 xs e2) (expr ~max_level:2 xs t)
          | Syntax.Refl e -> print ~at_level:1 "refl %t" (expr ~max_level:0 xs e)
          | Syntax.Transport (a, p, e) -> print ~at_level:1 "transport %t %t %t" (expr ~max_level:0 xs a) (expr ~max_level:0 xs p) (expr ~max_level:0 xs e)
          | Syntax.Nat -> print "nat"
          | Syntax.Zero -> print "0"
          | Syntax.Succ e ->
             (match to_int e with
               | Some n -> print "%d" (n + 1)
               | None -> print ~at_level:1 "succ %t" (expr ~max_level:0 xs e))
          | Syntax.NatRec (a, x, f, n) ->
            print ~at_level:1 "natrec %t %t %t %t"
              (expr ~max_level:0 xs a)
              (expr ~max_level:0 xs x)
              (expr ~max_level:0 xs f)
              (expr ~max_level:0 xs n)
          | Syntax.Pi (x, t1, t2) -> print ~at_level:3 "%t" (pi xs x t1 t2)
          | Syntax.Lambda (x, t, e) -> print ~at_level:3 "%t" (lambda xs x t e)
          | Syntax.App (e1, e2) -> print ~at_level:1 "%t@ %t" (expr ~max_level:1 xs e1) (expr ~max_level:0 xs e2)
          | Syntax.Ascribe (e, t) -> print ~at_level:4 "%t : %t" (expr ~max_level:3 xs e) (expr ~max_level:3 xs t)
          | Syntax.Type -> print "Type"
          | Syntax.Sort -> print "Sort"
          | Syntax.TyWtn (e, t) -> print "<witness>"
          | Syntax.EqWtn (e1, e2, t)-> print "<witness>"
          | Syntax.TyJdg (e, t) -> print ~at_level:3 "%t :: %t" (expr ~max_level:2 xs e) (expr ~max_level:2 xs t)
          | Syntax.EqJdg (e1, e2, t) -> print ~at_level:3 "%t == %t @@ %t"
                                          (expr ~max_level:2 xs e1) (expr ~max_level:2 xs e2) (expr ~max_level:2 xs t)
  in
    expr ?max_level xs e ppf
    
let expr' xs e ppf = expr xs (Common.nowhere e) ppf

let operation ?max_level xs (op, _) ppf =
  match op with
    | Syntax.Inhabit t -> print ppf "inhabit %t" (expr ~max_level:2 xs t)
    | Syntax.Infer e -> print ppf "infer %t" (expr ~max_level:2 xs e)
    | Syntax.HasType (e, t) ->
      print ppf "check %t %t" (expr  ~max_level:2 xs e) (expr ~max_level:2 xs t)
    | Syntax.Equal (e1, e2, t) ->
      print ppf "equal %t %t %t"
        (expr ~max_level:2 xs e1) (expr ~max_level:2 xs e2) (expr ~max_level:2 xs t)

(* XXX temporarily changed
let operation ?max_level xs (op, _) ppf =
  match op with
    | Syntax.Inhabit t -> print ppf "? :: %t" (expr ~max_level:2 xs t)
    | Syntax.Infer e -> print ppf "%t :: ?" (expr xs e)
    | Syntax.HasType (e, t) ->
      print ppf "[ %t :: %t ]" (expr  ~max_level:2 xs e) (expr ~max_level:2 xs t)
    | Syntax.Equal (e1, e2, t) ->
      print ppf "%t == %t @@ %t"
        (expr ~max_level:2 xs e1) (expr ~max_level:2 xs e2) (expr ~max_level:2 xs t)
*)

let rec computation ?max_level xs (c, _) ppf =
  match c with
    | Syntax.Return e -> print ppf "return %t" (expr ~max_level:4 xs e)
    | Syntax.Abstraction  (x, t, c) -> print ppf "fun %s : %t => %t" x (expr ~max_level:2 xs t) (computation (x::xs) c)
    | Syntax.Operation op -> print ppf "[ %t ]" (operation ?max_level xs op)
    | Syntax.Handle (c, h) -> print ppf "handle %t with <handler>" (computation xs c)
    | Syntax.Let (x, c1, c2) -> print ~at_level:3 ppf "let x := %t in %t" (computation ~max_level:2 xs c1) (computation (x::xs) c2)

let rec value ?max_level xs v ppf =
  match v with
    | Value.EqWtn (e1, e2, t) ->
      print ppf ~at_level:3 "%t == %t @@ %t"
        (expr ~max_level:2 xs e1) (expr ~max_level:2 xs e2) (expr ~max_level:2 xs t)
    | Value.TyWtn (e, t) -> print ppf "%t :: %t" (expr ~max_level:2 xs e) (expr ~max_level:2 xs t)
    | Value.Lambda (x, t, v) -> print ppf "forall %s : %t, %t" x (expr ~max_level:2 xs t) (value (x::xs) v)

let rec result ?max_level xs r ppf =
  match r with
    | Value.Value v -> print ppf "%t" (value xs v)
    | Value.Operation (op, _) -> print ppf "%t ..." (operation xs op)
    | Value.Abstraction (x, t, r, _) ->
      print ppf "fun %s : %t => %t ..." x (expr ~max_level:2 xs t) (result (x::xs) r)
    | Value.Definition (x, t, e, r, _) ->
      print ppf "let %s := %t : %t in %t ..." x (expr ~max_level:2 xs e) (expr ~max_level:2 xs t) (result (x::xs) r)

(** Support for printing of errors, warning and debugging information. *)

let verbosity = ref 2

let message ?(loc=Common.Nowhere) msg_type v =
  if v <= !verbosity then
    begin
      Format.eprintf "%s at %t:@\n@[" msg_type (position loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@]@.") Format.err_formatter
    end
  else
    Format.ifprintf Format.err_formatter

let error (loc, err_type, msg) = message ~loc (err_type) 1 "%s" msg
let warning msg = message "Warning" 2 msg
let debug msg = message "Debug" 3 msg
