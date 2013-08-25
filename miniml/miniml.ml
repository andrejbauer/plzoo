module MiniML = Toplevel.Make(struct
  type toplevel = Syntax.toplevel_cmd

  (** A context describing the types of globally defined values. *)
  type context = (Syntax.name * Syntax.ty) list

  (** A runtime environment describing globally defined values. *)
  type runtime = (Syntax.name * Machine.mvalue) list

  type environment = context * runtime

  let initial_environment = ([], [])

  let prompt = "# "

  let more_prompt = "  "

  let file_parser = Parser.file Lexer.token

  let toplevel_parser = Parser.toplevel Lexer.token

  let name = "miniml"

  let greeting = "miniml"

  let options = []

  let help_directive = None

  (** [exec interactive (ctx, env) cmd] executes the toplevel command [cmd] and returns
      the new context-environment pair and a string representing the result of
      evaluation. *)
  let exec interactive (ctx, env) = function
    | Syntax.Expr e ->
    (* check the type of [e], compile it, and run it. *)
      let ty = Type_check.type_of ctx e in
      let frm = Compile.compile e in
      let v = Machine.run frm env in
        if interactive then
          print_endline ("- : " ^ (Syntax.string_of_type ty) ^ " = " ^ (Machine.string_of_mvalue v)) ;
        (ctx, env)
    | Syntax.Def (x, e) ->
      (* check the type of [e], compile it, run it, and return a new
	 context-environemtn pair with [x] defined as [e]. *)
      let ty = Type_check.type_of ctx e in
      let frm = Compile.compile e in
      let v = Machine.run frm env in
        if interactive then
          print_endline (x ^ " : " ^ (Syntax.string_of_type ty) ^ " = " ^ (Machine.string_of_mvalue v)) ;
	((x,ty)::ctx, (x,v)::env)

  let read_more str =
    let i = ref (String.length str - 1) in
      while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
      !i < 1 || (str.[!i] <> ';' || str.[!i - 1] <> ';')
end) ;;

MiniML.main ()
