open Syntax
module T = Type

let bold fmt s = Format.fprintf fmt "@<0>%s%s@<0>%s" "\027[1m" s "\027[0m"

let constant fmt = function
  | Int i -> Format.pp_print_int fmt i
  | Plus -> bold fmt "+"
  | NewRef -> bold fmt "new"
  | Get -> bold fmt "!"
  | Set -> bold fmt ":="

let indice_array = [|"₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"|]
let rec digits fmt i =
  if i < 0 then
    Format.pp_print_string fmt "₋"
  else if i < 10 then
    Format.pp_print_string fmt indice_array.(i)
  else begin
    digits fmt (i/10) ;
    Format.pp_print_string fmt indice_array.(i mod 10)
  end

let name fmt {Name. name ; id } =
  Format.fprintf fmt "%s%a" name  digits id

let rec value
  = fun fmt -> function
    | Constant c -> constant fmt c
    | Lambda (n,e) ->
      Format.fprintf fmt "@[<2>%a %a %a@ %a@]"
        bold "fun"
        name n
        bold "->"
        expr e
    | Ref { contents } -> Format.fprintf fmt "{%a}" value contents
    | Y -> Format.fprintf fmt "Y"

and expr
  = fun fmt -> function
    | V v -> value fmt v
    | Var v -> name fmt v
    | App (f,e) ->
      Format.fprintf fmt "@[<2>@[%a@]@ %a@]"
        expr_with_paren f
        Format.(pp_print_list ~pp_sep:pp_print_space expr_with_paren) e
    | Let (n,e1,e2) ->
      Format.fprintf fmt "@[@[<2>%a %a %a@ %a@]@ %a@ %a@]"
        bold "let" name n
        bold "=" expr e1
        bold "in" expr e2

and expr_with_paren fmt x =
  let must_have_paren = match x with
    | App _ -> true
    | Let _ -> true
    | V (Lambda _) -> true
    | _ -> false
  in
  Format.fprintf fmt
    (if must_have_paren then "@[(%a@])" else "%a") expr x 

let rec typ_need_paren = function
  | T.Arrow _ -> true
  | T.Var { contents = Link t } -> typ_need_paren t
  | _ -> false

let rec tyvar
  = fun fmt -> function
  | T.Unbound (n,_) -> Format.fprintf fmt "'_%a" name n
  | T.Link t -> typ_with_paren fmt t

and typ
  = fun fmt -> function
  | T.Const n -> name fmt n
  | T.App (f,e) -> Format.fprintf fmt "@[<2>%a@ %a@]" typ_with_paren e  typ f
  | T.Arrow (a,b) -> Format.fprintf fmt "%a -> %a" typ_with_paren a  typ b
  | T.Var { contents = x } -> tyvar fmt x
  | T.GenericVar n -> Format.fprintf fmt "'%a" name n

and typ_with_paren fmt x =
  let must_have_paren = match x with
    | T.Arrow _ -> true
    | _ -> false
  in
  Format.fprintf fmt
    (if must_have_paren then "@[(%a@])" else "%a") typ x

let typ_env fmt env =
  let print_env fmt e =
    Format.pp_print_list
      ~pp_sep:Format.pp_print_cut
      (fun fmt (k,ty) -> Format.fprintf fmt "%a: %a" name k  typ ty)
      fmt
    @@ T.Env.M.bindings e
  in
  Format.fprintf fmt "Server:@;<1 2>@[<v>%a@]@.Client:@;<1 2>@[<v>%a@]@."
    print_env env
