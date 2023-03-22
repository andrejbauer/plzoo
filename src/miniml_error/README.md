An extension of [miniML](../miniml) with division which aborts execution upon division by
zero with an `error` value The error cannot be caught or thrown explicitly, so it is not a
proper exception.

Example interaction, see also the file `example.miniml_error`:

    miniML+error> 3 + (if 5 < 6 then 10 else 100);;
    - : int = 13
    miniML+error> let x = 14;;
    x : int = 14
    miniML+error> let fact = fun f (n : int) : int is if n = 0 then 1 else n * f (n-1);;
    fact : int -> int = <fun>
    miniML+error> fact 10;;
    - : int = 3628800
    miniML+error> 12/4;;
    - : int = 3
    miniML+error> 1/0;;
    - : int = error
