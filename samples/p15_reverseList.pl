% rules reverse a list
reverse([], Z, Z).
reverse([X|T], Z, Ans) :- reverse(T, Z, [X|Ans]).
