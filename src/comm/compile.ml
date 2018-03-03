open Machine

let compile cmd =
  let location env x =
    let rec index k = function
    | [] -> Zoo.error ~kind:"compilation error" "unknown variable %s" x
    | y :: ys -> if x = y then k else index (k+1) ys
    in
    index 0 env
  in

  let rec expression env = function

    | Syntax.Variable x ->
       let k = location env x in
       [GET k]

    | Syntax.Numeral k ->
       [PUSH k]

    | Syntax.Plus (e1, e2) ->
       (expression env e1) @ (expression env e2) @ [ADD]

    | Syntax.Minus (e1, e2) ->
       (expression env e1) @ (expression env e2) @ [SUB]

    | Syntax.Times (e1, e2) ->
       (expression env e1) @ (expression env e2) @ [MUL]

    | Syntax.Divide (e1, e2) ->
       (expression env e1) @ (expression env e2) @ [DIV]

    | Syntax.Remainder (e1, e2) ->
       (expression env e1) @ (expression env e2) @ [MOD]
  in

  let rec boolean env = function

  | Syntax.True -> [PUSH 1]

  | Syntax.False -> [PUSH 0]

  | Syntax.Equal (e1, e2) ->
     (expression env e1) @ (expression env e2) @ [EQ]

  | Syntax.Less (e1, e2) ->
     (expression env e1) @ (expression env e2) @ [LT]

  | Syntax.And (b1, b2) ->
     (boolean env b1) @ (boolean env b2) @ [AND]

  | Syntax.Or (b1, b2) ->
     (boolean env b1) @ (boolean env b2) @ [OR]

  | Syntax.Not b ->
     (boolean env b) @ [NOT]
  in

  let rec command env = function
  | Syntax.New (x, e, c) ->
     let e' = expression env e in
     let c' = command (x :: env) c in
     e' @ [VPUSH] @ c' @ [VPOP]

  | Syntax.Skip ->
     [NOOP]

  | Syntax.Print e ->
     (expression env e) @ [PRINT]

  | Syntax.Assign (x, e) ->
     (expression env e) @ [SET (location env x)]

  | Syntax.Sequence (c1, c2) ->
     (command env c1) @ (command env c2)

  | Syntax.Conditional (b, c1, c2) ->
     let c1' = command env c1 in
     let c2' = command env c2 in
     (boolean env b) @ [JMPZ (List.length c1' + 1)] @ c1' @ [JMP (List.length c2')] @ c2'

  | Syntax.While (b, c) ->
     let c' = command env c in
     let b' = boolean env b in
     let n = List.length c' in
     b' @ [JMPZ (n + 1)] @ c' @ [JMP (-(List.length b' + 2 + n))]
  in

  Array.of_list (command [] cmd)
