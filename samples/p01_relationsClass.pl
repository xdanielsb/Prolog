
% facts

man(daniel).
man(julian).
girl(ana).
girl(tris).

isaPair( X,Y) :- man(X), girl(Y),!.
isaPair( X,Y) :-  isaPair(Y, X).

/* Queries
?- man(daniel).
?- girl(liz).
?- man(X)
?- man(X);girl(X)
?- man(X),girl(X).
*/
