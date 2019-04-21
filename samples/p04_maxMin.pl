% rules

getMax(X,Y) :- X >= Y, write( X ).
getMax(X,Y) :- X < Y, write( Y ).
