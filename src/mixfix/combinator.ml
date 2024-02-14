
open Parser


let rec tighter (p:Precedence.t) = function
  | [] -> []
  | (x, _)::_ as r when x >= (fst p) -> r
  | _::rest -> tighter p rest
;;



let create_parser (precs:Precedence.graph): (Presyntax.expr, Syntax.expr) Parser.t =
  (if_then_else_endif numeral @@> eof)