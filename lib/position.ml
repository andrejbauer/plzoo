(** Position in source code. For each type in the abstract syntax we define two versions
    [t] and [t']. The former is the latter with a position tag. For example, [expr = expr'
    * position] and [expr'] is the type of expressions (without positions). 
*)
type t =
  | Position of Lexing.position * Lexing.position (** delimited position *)
  | Nowhere (** unknown position *)

(** [nowhere e] is the expression [e] without a source position. It is used when
    an expression is generated and there is not reasonable position that could be
    assigned to it. *)
let nowhere x = (x, Nowhere)

(** Convert a position as presented by [Lexing] to [position]. *)
let position_of_lex lex =
  Position (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(** Print the given source code position. *)
let position loc ppf =
  match loc with
  | Nowhere ->
      Format.fprintf ppf "unknown position"
  | Position (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in

      if String.length filename != 0 then
        Format.fprintf ppf "file %S, line %d, charaters %d-%d" filename begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1) begin_char end_char

