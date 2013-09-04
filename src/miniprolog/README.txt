Miniprolog is a minimalist prolog interpeter. It is not designed for
speed but rather for demonstration purposes.

The terms of the language consist of:

* constants, which are strings of alphanumeric characters, starting
  with a lower case letter.

* variables, which are strings of alphanumeric characters, starting
  with a upper case letter.

* composite terms of the form f(t1, ..., tn) where f is a constant
  and t1, ..., tn are terms.

Examples of terms are: x, y, X, A, son(abel, adam), less(X,plus(X,one)).

You may type in assertions and queries. An atomic assertion is written
as a term followed by a period. An inference rule (complex assertion)
is written as

goal :- subgoal1, ..., subgoalN.

where goal, subgoal1, ..., subgoalN are terms. A query is written as
(including the "?-"):

?- query.

where query is a term. You may type assertions and queries at the
command line or place them in a file. See the enclosed examples.

Sample session:

Miniprolog. Press Ctrl-D to exit.
Input syntax: 
   ?- query.           Make a query.
   a(t1, ..., tn).     Assert an atomic proposition.
   A :- B1, ..., Bn.   Assert an inference rule.
   $quit               Exit interpreter.
   $use "filename"     Execute commands from a file.
Prolog> it_is_raining.
Prolog> no_umbrella.
Prolog> i_am_wet :- it_is_raining, no_umbrella.
Prolog> ?- i_am_wet.
Yes
Prolog> ?- i_am_dry.
No
Prolog> child(luke, vader).
Prolog> child(leia, vader).
Prolog> ?- child(X, vader).
X = luke
more? (y/n) [y] 
X = leia
more? (y/n) [y] 
No
Prolog> sibling(X,Y) :- child(X,Z), child(Y,Z).
Prolog> ?- sibling(A,B).
A = luke
B = luke
more? (y/n) [y] 
A = luke
B = leia
more? (y/n) [y] 
A = leia
B = luke
more? (y/n) [y] 
A = leia
B = leia
more? (y/n) [y] 
No
Prolog> $quit

Good bye.
