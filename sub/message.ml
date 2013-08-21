(** Sporoèila o napakah *)

open Lexing

(** [lexer_from_file fn ch] vrne tok za lekser, ki èrpa znake
    iz vhodnega toka [ch]. Ime datoteke se nastavi na [fn]. *)
let lexer_from_channel fn ch =
  let lex = Lexing.from_channel ch in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1; } ;
    lex

(** [lexer_from_string str] vrne tok za lekser, ki èrpa znake
    iz niza [str]. Ime datoteke se nastavi na [""]. *)
let lexer_from_string str =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = ""; pos_lnum = 1; } ;
    lex

let string_of_position {pos_fname=fn; pos_lnum=ln; pos_bol=bol; pos_cnum=cn} =
  let c = cn - bol in
    if fn = "" then
      "Character " ^ string_of_int c
    else
      "File \"" ^ fn ^ "\", line " ^ string_of_int ln ^ ", character " ^ string_of_int c

let string_of msg pos = string_of_position pos ^ ":\n" ^ msg

let syntax_error {lex_curr_p=pos} = string_of "Syntax error" pos

let report = print_endline
