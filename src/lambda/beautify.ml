(** Renaming of bound variables for pretty printing. *)

open Syntax

(** Split a variable name into base and numerical postfix, e.g.,
   ["x42"] is split into [("x", 42)]. *)
let split s =
  let n = String.length s in
  let i = ref (n - 1) in
    while !i >= 0 && '0' <= s.[!i] && s.[!i] <= '9' do decr i done ;
    if !i < 0 || !i = n - 1 
    then (s, None)
    else
      let k = int_of_string (String.sub s (!i+1) (n - !i - 1)) in
        (String.sub s 0 (!i+1), Some k)

(** Given a variable [x] and a list of variable names [xs], find a variant of [x] which
    does not appear in [xs]. *)
let refresh x xs =
  if not (List.mem x xs)
  then x
  else
    let (y, k) = split x in
    let k = ref (match k with Some k -> k | None -> 0) in
      while List.mem (y ^ string_of_int !k) xs do incr k done ;
      y ^ string_of_int !k
