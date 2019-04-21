% facts

female(maria).
female(camila).
male(carlos).
male(juan).
parent(juan, maria).
parent(camila, maria).
parent(camila, carlos).
parent(juan, carlos).


mother(X,Y) :- female(Y),parent(X,Y).
sister(X,Y) :- female(Y), parent(X, Z), parent(Y, Z), Y\==X.
hasParent(Y) :- parent(Y,_).
