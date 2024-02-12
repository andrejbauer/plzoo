module Mixfix = Zoo.Main(struct

  let name = "Mixfix"

  type command = Syntax.toplevel_cmd

  type environment = {
    operators: Precedence.graph;
    context: (Syntax.name * Presyntax.htype) list;
    env: Interpret.environment;
  }

  let print_depth = ref 100

  let options = [("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]

  let initial_environment: environment = {
    operators = Precedence.new_graph ();
    context = [];
    env = [];
  }

  let file_parser = Some (fun environ  s -> Mixer.file environ (Preparser.file Lexer.token s) )

  let toplevel_parser = Some (fun environ  s -> Mixer.toplevel_cmd environ (Preparser.toplevel Lexer.token s) )

  let exec (state:environment) = function
    | Syntax.Expr e ->
      (* type check [e], evaluate, and print result *)
       let ty = Type_check.type_of state.context e in
       let v = Interpret.interp state.env e in
       Zoo.print_info "- : %s = " (Presyntax.string_of_type ty) ;
       Interpret.print_result !print_depth v ;
       Zoo.print_info "@." ;
       {state with env = state.env}
    | Syntax.Def (x, e) ->
       (* type check [e], and store it unevaluated! *)
       let ty = Type_check.type_of state.context e in
       Zoo.print_info "val %s : %s@." x (Presyntax.string_of_type ty) ;
      {state with context = (x,ty)::state.context; env = (x, ref (Interpret.VClosure (state.env,e)))::state.env}
    | Syntax.Mixfix (operator)->
       (* Ad operator x with precedence prec and expression e to environment.operators *)
      {state with operators = Precedence.add_operator state.operators operator }
    | Syntax.Quit -> raise End_of_file
end) ;;

Mixfix.main () ;;
