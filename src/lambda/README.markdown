An implementation of the untyped λ-calculus.

The λ-calculus is an equational theory and so does not by itself enforce a notion of
computation. There are several strategies for normalizing redices, which in `lambda` can
be controlled with directives:

* `#eager` - reduce arguments of applications
* `#lazy` - do not reduce arguments of applications
* `#deep` - reduce inside abstractions
* `#shallow` - do not reduce inside abstractions

We can combine these to get various reduction strategies:

|            |    `#eager`      |        `#lazy`        |
|-----------:|:----------------:|:---------------------:|
| `#shallow` | weak normal form | weak head normal form |
| `#deep`    | normal form      | head normal form      |

In terms of programming language terminology, weak normal form corresponds approximately
to call by value and the weak head normal form to call by name.

##### Example interaction

The file `example.lambda` contains an example session which defines booleans, numbers, and
lists. You can use it as follows:

    $ ./lambda.native -l example.lambda 
    pair is defined.
    first is defined.
    second is defined.
    K is defined.
    true is defined.
    false is defined.
    if is defined.
    and is defined.
    or is defined.
    not is defined.
    fix is defined.
    error is a constant.
    nil is defined.
    cons is defined.
    head is defined.
    tail is defined.
    match is defined.
    map is defined.
    fold is defined.
    0 is defined.
    1 is defined.
    2 is defined.
    3 is defined.
    4 is defined.
    5 is defined.
    6 is defined.
    7 is defined.
    8 is defined.
    9 is defined.
    10 is defined.
    succ is defined.
    + is defined.
    * is defined.
    ** is defined.
    iszero is defined.
    pred is defined.
    == is defined.
    fact is defined.
    <= is defined.
    >= is defined.
    < is defined.
    > is defined.
    mu is defined.
    / is defined.
    | is defined.
    all is defined.
    prime is defined.
    lambda -- programming languages zoo
    Type Ctrl-D to exit
    lambda> prime 3
    λ x _ . x
    lambda> prime 4
    λ _ y . y
    lambda> prime 5
    λ x _ . x
    lambda> prime 6
    λ _ y . y
    lambda> prime 7
    λ x _ . x
    lambda> + 2 3
    λ f x . 2 f (3 f x)
    lambda> :eager
    I will evaluate eagerly.
    lambda> :deep
    I will evaluate deeply.
    lambda> + 2 3
    λ f x . f (f (f (f (f x))))
    lambda> :lazy
    I will evaluate lazily.
    lambda> + 2 3
    λ f x . f (f (3 f x))
