module Boa = Zoo.Main(struct

let name = "Boa"

type command = Syntax.toplevel_cmd

type environment = Eval.env

let options = []

let initial_environment = []

let file_parser = Some (fun _ -> Parser.file Lexer.token)

let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.token)

let exec env = function

  | Syntax.Expr e ->
      (* evaluate [e] *)
      let v = Eval.eval env e in
        Format.printf "%t@." (Eval.print_value v) ;
        env

  | Syntax.Def (x, e) ->
      (* define a new global value *)
      let v = Eval.eval env e in
      Format.printf "%s = @[%t@]@." x (Eval.print_value v) ;
      (x,v)::env
end);;

Boa.main() ;;
