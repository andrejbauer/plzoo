type location = { loc_line : int; loc_start : int; loc_end : int }

exception Parse of location * string

let lineno = ref 0

let loc_here n = 
  { loc_line = !lineno;
    loc_start = Parsing.rhs_start n;
    loc_end = Parsing.rhs_end n
  }
