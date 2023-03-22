Miniprolog is a minimalist prolog interpeter. It does not contain cuts, arithmetic, lists,
or equality -- just basic Horn clauses.

The terms of the language consist of:

* constants, which are strings of alphanumeric characters, starting
  with a lower case letter.
* variables, which are strings of alphanumeric characters, starting
  with a upper case letter.
* composite terms of the form `f(t1, ..., tn)` where `f` is a constant
  and `t1`, …, `tn` are terms.

Examples of terms are: `x`, `y`, `X`, `A`, `son(abel, adam)`, `less(X, plus(X, one))`.

You may type in assertions and queries. An atomic assertion is written
as a term followed by a period,

    assertion.

an inference rule (complex assertion) is written as

    goal :- subgoal1, ..., subgoalN.

where `assertion`, `goal`, `subgoal1`, …, `subgoalN` are terms. A query is written as
(including the "`?-`")

    ?- query.

where query is a term. You may type assertions and queries at the
command line or place them in a file. See the enclosed examples.

Sample session, see also the file `example.miniprolog`.

    miniProlog -- programming languages zoo
    Type Ctrl-D to exit
    miniProlog> it_is_raining.
    miniProlog> no_umbrella.
    miniProlog> i_am_wet :- it_is_raining, no_umbrella.
    miniProlog> ?- i_am_wet.
    Yes
    miniProlog> ?- i_am_dry.
    No
    miniProlog> child(luke, vader).
    miniProlog> child(leia, vader).
    miniProlog> ?- child(X, vader).
    X = luke
    more? (y/n) [y] y
    X = leia
    more? (y/n) [y] y
    No
    miniProlog> sibling(X,Y) :- child(X,Z), child(Y,Z).
    miniProlog> ?- sibling(A,B).
    A = luke
    B = luke
    more? (y/n) [y] y
    A = luke
    B = leia
    more? (y/n) [y] y
    A = leia
    B = luke
    more? (y/n) [y] y
    A = leia
    B = leia
    more? (y/n) [y] y
    No
    miniProlog> 
