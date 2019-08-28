(** Compilation to machine code. *)
open Machine

(** Compile the given command. *)
let compile cmd =
  (* We keep around a context, which is a list of currently valid variables,
     with more recently declared variables at the beginning of the list. *)

  (* Compute the RAM location where variable [x] is stored, given the context [ctx].
     The location of a variable is its de Bruijn level: the first declared variable
     is at location 0, the second at location 1, etc.
   *)
  let location ctx x =
    let rec index k = function
    | [] -> Zoo.error ~kind:"compilation error" "unknown variable %s" x
    | y :: ys -> if x = y then k else index (k-1) ys
    in
    index (List.length ctx - 1) ctx
  in

  (* Compile an expression in the given context [ctx]. *)
  let rec expression ctx = function

    | Syntax.Variable x ->
       let k = location ctx x in
       [GET k]

    | Syntax.Numeral k ->
       [PUSH k]

    | Syntax.Plus (e1, e2) ->
       (expression ctx e1) @ (expression ctx e2) @ [ADD]

    | Syntax.Minus (e1, e2) ->
       (expression ctx e1) @ (expression ctx e2) @ [SUB]

    | Syntax.Times (e1, e2) ->
       (expression ctx e1) @ (expression ctx e2) @ [MUL]

    | Syntax.Divide (e1, e2) ->
       (expression ctx e1) @ (expression ctx e2) @ [DIV]

    | Syntax.Remainder (e1, e2) ->
       (expression ctx e1) @ (expression ctx e2) @ [MOD]
  in

  (** Compile a boolea expression in the given context. *)
  let rec boolean ctx = function

  | Syntax.True -> [PUSH 1]

  | Syntax.False -> [PUSH 0]

  | Syntax.Equal (e1, e2) ->
     (expression ctx e1) @ (expression ctx e2) @ [EQ]

  | Syntax.Less (e1, e2) ->
     (expression ctx e1) @ (expression ctx e2) @ [LT]

  | Syntax.And (b1, b2) ->
     (boolean ctx b1) @ (boolean ctx b2) @ [AND]

  | Syntax.Or (b1, b2) ->
     (boolean ctx b1) @ (boolean ctx b2) @ [OR]

  | Syntax.Not b ->
     (boolean ctx b) @ [NOT]
  in

  (** Compile the given command in the given context [ctx]. *)
  let rec command ctx = function
  | Syntax.New (x, e, c) ->
     let e' = expression ctx e in
     let ctx = x :: ctx in
     let c' = command ctx c in
     let k = location ctx x in
     e' @ [SET k] @ c'

  | Syntax.Skip ->
     [NOOP]

  | Syntax.Print e ->
     (expression ctx e) @ [PRINT]

  | Syntax.Assign (x, e) ->
     (expression ctx e) @ [SET (location ctx x)]

  | Syntax.Sequence (c1, c2) ->
     (command ctx c1) @ (command ctx c2)

  | Syntax.Conditional (b, c1, c2) ->
     let c1' = command ctx c1 in
     let c2' = command ctx c2 in
     (boolean ctx b) @ [JMPZ (List.length c1' + 1)] @ c1' @ [JMP (List.length c2')] @ c2'

  | Syntax.While (b, c) ->
     let c' = command ctx c in
     let b' = boolean ctx b in
     let n = List.length c' in
     b' @ [JMPZ (n + 1)] @ c' @ [JMP (-(List.length b' + 2 + n))]
  in

  Array.of_list (command [] cmd)
