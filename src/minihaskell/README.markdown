A lazy functional language with following features:

* integers with arithmetical operations `*`, `+`, `-`, `/`, `%`
* booleans with conditional statements and comparison `=`, `<` of integers
* ordered pairs
* lists
* functions
* general recursion (fixpoint operator)

#### Examples

The file primes.minhs defines the infinite list of prime numbers. You
can load it and try it as follows:

    $ ./minihaskell.byte prime.minhs 
    val not : bool -> bool
    val div : int -> int -> int
    val mod : int -> int -> int
    val notmultiple : int -> int -> bool
    val map : (int -> int) -> int list -> int list
    val filter : (int -> bool) -> int list -> int list
    val nth : int list -> int -> int
    val nat : int list
    val nat2 : int list
    val sieve : int list -> int list
    val primes : int list
    MiniHaskell. Press Ctrl-D to exit.
    MiniHaskell> primes
    - : int list = 2 :: 3 :: 5 :: 7 :: 11 :: 13 :: 17 :: 19 :: 23 ::
      29 :: 31 :: 37 :: 41 :: 43 :: 47 :: 53 :: 59 :: 61 :: 67 :: 71
      :: 73 :: 79 :: 83 :: 89 :: 97 :: 101 :: 103 :: 107 :: 109 :: 113
      :: 127 :: 131 :: 137 :: 139 :: 149 :: 151 :: 157 :: 163 :: 167
      :: 173 :: 179 :: 181 :: 191 :: 193 :: 197 :: 199 :: 211 :: 223
      :: 227 :: 229 :: 233 :: 239 :: 241 :: 251 :: 257 :: 263 :: 269
      :: 271 :: 277 :: 281 :: 283 :: 293 :: 307 :: 311 :: 313 :: 317
      :: 331 :: 337 :: 347 :: 349 :: 353 :: 359 :: 367 :: 373 :: 379
      :: 383 :: 389 :: 397 :: 401 :: 409 :: 419 :: 421 :: 431 :: 433
      :: 439 :: 443 :: 449 :: 457 :: 461 :: 463 :: 467 :: 479 :: 487
      :: 491 :: 499 :: 503 :: 509 :: 521 :: 523 :: ... :: ...
    MiniHaskell> 
    Good bye.
