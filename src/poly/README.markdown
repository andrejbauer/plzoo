A simple lazy purely functional language with parametric
polymorphism. The implementation contains a parser, type inference,
and an efficient interpreter. The language has integers, booleans,
lists, pairs, functions, and a general fixpoint operator.

Poly is very similar to MiniHaskell. The only difference is that it
infers polymorphic types.

#### Examples

The file `list.minply` defines basic operations on lists:

    ./poly.native list.minply 
    val range : int -> int -> int list
    val map : ('a -> 'b) -> 'a list -> 'b list
    val append : 'a list -> 'a list -> 'a list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val flatten : ('a list) list -> 'a list
    val zip2 : 'a list -> 'a list -> 'a list
    val zip : ('a list) list -> 'a list
