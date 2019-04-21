%rules
add(X,Y, Ans) :- Ans is X+Y, write(Ans).
mul(X,Y, Ans) :- Ans is X*Y, write(Ans).
div(X,Y, Ans) :- Y \== 0, Ans is X/Y, write(Ans); Y = 0,write("oo").
sub(X,Y, Ans) :- Ans is X-Y, write(Ans).
