# Family tree in miniprolog

# Basic assertions (the names of people are Slovene)
female(maja).
female(jozefa).
male(franc).
male(marko).
male(hinko).
female(ana).
female(marija).
male(polde).
male(martin).
male(primoz).
female(petra).
female(nada).

# child(X,Y) means that X is the child of Y
child(maja, jozefa).
child(maja, franc).
child(marko, jozefa).
child(marko, franc).
child(jozefa, hinko).
child(jozefa, ana).
child(franc, marija).
child(franc, polde).
child(janez, marija).
child(janez, polde).
child(martin, hinko).
child(martin, ana).
child(primoz, martin).
child(petra, martin).
child(primoz, nada).
child(petra, nada).

son(X,Y) :- male(X), child(X,Y).
mother(X,Y) :- female(Y), child(X,Y).
grandson(X,Y) :- male(X), child(X,Z), child(Z,Y).
sister(X,Y) :- female(Y), child(X,Z), child(Y,Z).
aunt(X,Y) :- child(X,Z), sister(Z,Y).

descendant(X,Y) :- child(X,Y).
descendant(X,Y) :- child(X,Z), descendant(Z,Y).

