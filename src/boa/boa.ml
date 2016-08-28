module Boa = Zoo.Main(struct

let name = "Boa"

type command = Syntax.toplevel_cmd

type environment = Eval.env

let options = []

let initial_environment = []

let read_more str =
  let i = ref (String.length str - 1) in
  while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
  !i < 1 || (str.[!i] <> ';' || str.[!i - 1] <> ';')

let file_parser = Some (Parser.file Lexer.token)

let toplevel_parser = Some (Parser.command Lexer.token)

let rec exec env = function

  | Syntax.Expr e ->
      (* evaluate [e] *)
      let v = Eval.eval env e in
	print_string (Eval.string_of_obj v) ;
	print_newline () ;
	env

  | Syntax.Def (x, e) ->
      (* define a new global value *)
      let v = Eval.eval env e in
	print_string (x ^ " = " ^ Eval.string_of_obj v) ;
	print_newline () ;
	(x,v)::env
end);;

Boa.main() ;;
